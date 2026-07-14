"""Empirical cancellation risk.

DB excludes cancelled trains from its delay statistics, and the Bahn-Vorhersage
model is trained only on non-cancelled stops (verified in their data prep), so
its distributions are conditional on the train running. This module supplies
P(cancel) as a separate branch.

Lookup keys, most to least specific (first hit wins):
    "<line>|<daytype>|<hourbucket>"   e.g. "RE 4|wd|morning"
    "<line>|<daytype>"                e.g. "RE 4|wd"
    "<line>"                          e.g. "RE 4"
    "<category>"                      e.g. "RE"
Values are smoothed empirical rates built by `trainpunct build-cancel-table`
from the piebro/deutsche-bahn-data monthly parquet (CC BY 4.0), see README.
Without a table, a configurable default rate applies.
"""

from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path

HOUR_BUCKETS = [(0, 6, "night"), (6, 10, "morning"), (10, 15, "midday"),
                (15, 20, "evening"), (20, 24, "late")]


def hour_bucket(hour: int) -> str:
    for lo, hi, name in HOUR_BUCKETS:
        if lo <= hour < hi:
            return name
    return "night"


def day_type(dt: datetime) -> str:
    return "we" if dt.isoweekday() >= 6 else "wd"


class CancelTable:
    def __init__(self, rates: dict[str, float] | None = None, default_rate: float = 0.03):
        self.rates = rates or {}
        self.default_rate = default_rate

    @classmethod
    def load(cls, path: str | Path, default_rate: float = 0.03) -> CancelTable:
        p = Path(path)
        if not p.exists():
            return cls(default_rate=default_rate)
        data = json.loads(p.read_text())
        return cls(rates=data.get("rates", {}), default_rate=default_rate)

    def rate(self, line_name: str | None, category: str | None, planned: datetime | None) -> float:
        line = (line_name or "").strip()
        cat = (category or "").strip()
        keys = []
        if planned is not None and line:
            keys.append(f"{line}|{day_type(planned)}|{hour_bucket(planned.hour)}")
            keys.append(f"{line}|{day_type(planned)}")
        if line:
            keys.append(line)
        if cat:
            keys.append(cat)
        for key in keys:
            if key in self.rates:
                return self.rates[key]
        return self.default_rate

    def p_journey_cancel(self, legs: list[tuple[str | None, str | None, datetime | None]]) -> float:
        """P(at least one leg cancelled); legs = [(line_name, category, planned_departure)]."""
        p_ok = 1.0
        for line, cat, planned in legs:
            p_ok *= 1.0 - self.rate(line, cat, planned)
        return 1.0 - p_ok


def build_table_from_piebro(
    parquet_paths: list[str | Path],
    corridor_evas: set[int] | None = None,
    smoothing_k: float = 20.0,
    out_path: str | Path = "cancel_table.json",
) -> dict:
    """Build the lookup from piebro/deutsche-bahn-data monthly parquet files.

    Relevant columns: eva, line_number, train_type, is_canceled, time.
    Smoothed with a pseudo-count prior towards the overall rate.
    Requires pyarrow (install extra: `pip install trainpunct[bootstrap]`).
    """
    import pyarrow.dataset as ds

    counts: dict[str, list[int]] = {}
    total = [0, 0]

    dataset = ds.dataset([str(p) for p in parquet_paths])
    cols = ["eva", "line_number", "train_type", "is_canceled", "time"]
    have = [c for c in cols if c in dataset.schema.names]
    for batch in dataset.to_batches(columns=have):
        rows = batch.to_pylist()
        for row in rows:
            eva = row.get("eva")
            if corridor_evas and eva is not None and int(eva) not in corridor_evas:
                continue
            cancelled = 1 if row.get("is_canceled") else 0
            t = row.get("time")
            dt = None
            if isinstance(t, str):
                try:
                    dt = datetime.fromisoformat(t)
                except ValueError:
                    dt = None
            elif isinstance(t, datetime):
                dt = t
            cat = str(row.get("train_type") or "").strip()
            line_no = str(row.get("line_number") or "").strip()
            line = f"{cat} {line_no}".strip()
            keys = [cat] if not line_no else [line, cat]
            if dt is not None and line_no:
                keys.insert(0, f"{line}|{day_type(dt)}")
                keys.insert(0, f"{line}|{day_type(dt)}|{hour_bucket(dt.hour)}")
            for key in keys:
                c = counts.setdefault(key, [0, 0])
                c[0] += cancelled
                c[1] += 1
            total[0] += cancelled
            total[1] += 1

    overall = total[0] / total[1] if total[1] else 0.03
    rates = {
        key: (c[0] + smoothing_k * overall) / (c[1] + smoothing_k)
        for key, c in counts.items()
        if c[1] >= 25
    }
    result = {"overall_rate": overall, "n_stops": total[1], "rates": rates}
    Path(out_path).write_text(json.dumps(result, indent=1, sort_keys=True))
    return result
