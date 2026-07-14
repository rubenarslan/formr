"""Delay distributions -> quantiles, and journey-level composition.

The predictor returns, per stop event, a pmf over the *deviation* from the
currently prognosed delay: index `offset` means "prognosis exactly right",
index 0 means "prognosis - offset". We convert to absolute delay (minutes vs
the planned time) by anchoring at delay_prognosed.

Tail caveat (from the training code): deviations are clipped to [-3, +30], so
the last bin means ">= prognosis + 30" and true extreme tails are underestimated.
Quantiles that land in the last bin therefore return its lower edge - they are
conservative minimums, flagged via `tail_open`.

Journey composition: a journey either works (all legs run, all transfers made)
-> conditional arrival pmf of its last leg; or it fails (a leg cancelled or a
transfer missed) -> you take the next option, arriving with *that* option's
effective distribution plus the schedule gap. Options are composed back-to-front
so each option's failure branch uses the next option's already-effective pmf.
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class Pmf:
    """Discrete distribution over integer minutes of delay (vs planned time)."""

    start: int  # delay value of probs[0]
    probs: list[float]

    def normalized(self) -> Pmf:
        s = sum(self.probs)
        if s <= 0:
            return Pmf(self.start, [1.0] + [0.0] * (len(self.probs) - 1))
        return Pmf(self.start, [p / s for p in self.probs])

    def quantile(self, q: float) -> tuple[int, bool]:
        """Smallest delay d with P(delay <= d) >= q. Second value: landed in open tail bin."""
        acc = 0.0
        for i, p in enumerate(self.probs):
            acc += p
            if acc >= q - 1e-12:
                return self.start + i, i == len(self.probs) - 1
        return self.start + len(self.probs) - 1, True

    def shift(self, minutes: int) -> Pmf:
        return Pmf(self.start + minutes, list(self.probs))

    def p_delay_greater(self, minutes: int) -> float:
        """P(delay > minutes)."""
        return sum(p for i, p in enumerate(self.probs) if self.start + i > minutes)


def pmf_from_prediction(row: list[float], offset: int, delay_prognosed: int) -> Pmf:
    return Pmf(start=delay_prognosed - offset, probs=list(row)).normalized()


def mixture(parts: list[tuple[float, Pmf]]) -> Pmf:
    parts = [(w, pmf) for w, pmf in parts if w > 1e-9]
    if not parts:
        return Pmf(0, [1.0])
    start = min(pmf.start for _, pmf in parts)
    end = max(pmf.start + len(pmf.probs) - 1 for _, pmf in parts)
    probs = [0.0] * (end - start + 1)
    for w, pmf in parts:
        for i, p in enumerate(pmf.probs):
            probs[pmf.start + i - start] += w * p
    return Pmf(start, probs).normalized()


@dataclass
class OptionInput:
    """One journey option, in composition order (earliest scheduled departure first)."""

    planned_arrival_min: int  # planned arrival at destination, minutes since midnight-ish anchor
    conditional: Pmf  # arrival delay pmf, conditional on running & connections made
    p_miss: float  # P(any transfer missed), from predictor transfer scores
    p_cancel: float  # P(any leg cancelled), empirical + live flags
    cancelled_now: bool = False  # already announced as cancelled


@dataclass
class OptionResult:
    conditional_q: dict[int, int] = field(default_factory=dict)
    effective_q: dict[int, int] = field(default_factory=dict)
    tail_open: bool = False
    p_miss: float = 0.0
    p_cancel: float = 0.0
    p_fail: float = 0.0
    effective: Pmf | None = None


QUANTILES = (0.5, 0.8, 0.95)


def compose_options(
    options: list[OptionInput], fallback_headway_min: int = 60
) -> list[OptionResult]:
    """Back-to-front composition; each failure branch re-plans onto the next option."""
    results: list[OptionResult] = [OptionResult() for _ in options]
    next_effective: Pmf | None = None
    next_planned: int | None = None

    for i in range(len(options) - 1, -1, -1):
        opt = options[i]
        p_fail = 1.0 - (1.0 - opt.p_miss) * (1.0 - opt.p_cancel)
        if opt.cancelled_now:
            p_fail = 1.0

        if next_effective is not None and next_planned is not None:
            gap = next_planned - opt.planned_arrival_min
            fallback = next_effective.shift(max(0, gap))
        else:
            fallback = opt.conditional.shift(fallback_headway_min)

        effective = mixture([(1.0 - p_fail, opt.conditional), (p_fail, fallback)])

        res = results[i]
        res.p_miss, res.p_cancel, res.p_fail = opt.p_miss, opt.p_cancel, p_fail
        res.effective = effective
        tail = False
        for q in QUANTILES:
            v, t1 = opt.conditional.quantile(q)
            res.conditional_q[int(q * 100)] = v
            ev, t2 = effective.quantile(q)
            res.effective_q[int(q * 100)] = ev
            tail = tail or t1 or t2
        res.tail_open = tail

        next_effective, next_planned = effective, opt.planned_arrival_min

    return results


def badge(
    eff_q50: int,
    eff_q80: int,
    p_fail: float,
    cancelled_now: bool,
    green_q80_max: int = 5,
    green_p_miss_max: float = 0.10,
    red_q50_min: int = 10,
    red_p_miss_min: float = 0.30,
) -> str:
    if cancelled_now:
        return "red"
    if eff_q50 > red_q50_min or p_fail > red_p_miss_min:
        return "red"
    if eff_q80 <= green_q80_max and p_fail < green_p_miss_max:
        return "green"
    return "amber"
