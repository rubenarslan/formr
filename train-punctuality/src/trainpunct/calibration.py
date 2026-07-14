"""Calibration metrics over logged predictions joined with realized outcomes."""

from __future__ import annotations

HORIZON_BUCKETS = [(0, 15, "0-15"), (15, 45, "15-45"), (45, 120, "45-120"), (120, 100000, "120+")]


def _bucket(horizon_min: int | None) -> str:
    h = horizon_min or 0
    for lo, hi, name in HORIZON_BUCKETS:
        if lo <= h < hi:
            return name
    return "120+"


def pinball(realized: float, predicted: float, q: float) -> float:
    diff = realized - predicted
    return q * diff if diff >= 0 else (q - 1.0) * diff


def summarize(history: list[dict]) -> dict:
    """history rows: prediction_log joined with outcome (see Store.history)."""
    delays = [r for r in history if not r.get("cancelled")]
    out: dict = {
        "n_resolved": len(history),
        "n_cancelled": sum(1 for r in history if r.get("cancelled")),
        "buckets": {},
        "db_bias": {},
    }

    for source in ("vendo", "iris"):
        key = f"db_forecast_{source}"
        rows = [r for r in delays if r.get(key) is not None and r.get("realized_delay") is not None]
        if rows:
            errs = [r["realized_delay"] - r[key] for r in rows]
            out["db_bias"][source] = {
                "n": len(rows),
                "mean_underestimate_min": round(sum(errs) / len(errs), 2),
                "p_worse_than_forecast": round(sum(1 for e in errs if e > 0) / len(errs), 3),
            }

    by_bucket: dict[str, list[dict]] = {}
    for r in delays:
        if r.get("realized_delay") is None:
            continue
        by_bucket.setdefault(_bucket(r.get("horizon_min")), []).append(r)

    for bucket, rows in sorted(by_bucket.items()):
        entry: dict = {"n": len(rows), "coverage": {}, "pinball": {}}
        for q_pct, q in ((50, 0.5), (80, 0.8), (95, 0.95)):
            col = f"q{q_pct}"
            usable = [r for r in rows if r.get(col) is not None]
            if not usable:
                continue
            entry["coverage"][col] = round(
                sum(1 for r in usable if r["realized_delay"] <= r[col]) / len(usable), 3
            )
            entry["pinball"][col] = round(
                sum(pinball(r["realized_delay"], r[col], q) for r in usable) / len(usable), 2
            )
        out["buckets"][bucket] = entry

    predicted_cancel = [r for r in history if r.get("p_cancel") is not None]
    if predicted_cancel:
        out["cancellation"] = {
            "n": len(predicted_cancel),
            "mean_predicted": round(
                sum(r["p_cancel"] for r in predicted_cancel) / len(predicted_cancel), 4
            ),
            "observed_rate": round(
                sum(1 for r in predicted_cancel if r.get("cancelled")) / len(predicted_cancel), 4
            ),
        }
    return out
