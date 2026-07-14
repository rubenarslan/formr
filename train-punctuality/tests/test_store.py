from datetime import UTC, datetime, timedelta

from trainpunct.store import Store


def _pred_row(ts: str, jhash: str, planned: str) -> dict:
    return {
        "ts": ts, "journey_hash": jhash, "day_kind": "mon_out", "leg_idx": 0,
        "trip_id": "t1", "fahrt_nr": "10426", "line": "RE 4", "station_eva": "8000251",
        "event": "arrival", "planned": planned, "db_forecast_vendo": 3,
        "db_forecast_iris": 7, "q50": 5, "q80": 9, "q95": 15,
        "eff_q50": 6, "eff_q80": 12, "eff_q95": 40, "p_miss": 0.1, "p_cancel": 0.02,
        "horizon_min": 30, "model_digest": "test",
    }


def test_roundtrip_and_unresolved(tmp_path):
    store = Store(tmp_path / "t.sqlite3")
    past = (datetime.now(UTC) - timedelta(hours=3)).isoformat(timespec="seconds")
    future = (datetime.now(UTC) + timedelta(hours=3)).isoformat(timespec="seconds")

    store.log_predictions([_pred_row("2026-07-20T05:00:00+00:00", "abc", past)])
    store.log_predictions([_pred_row("2026-07-20T05:10:00+00:00", "abc", past)])  # newer snapshot
    store.log_predictions([_pred_row("2026-07-20T05:00:00+00:00", "def", future)])

    cutoff = datetime.now(UTC).isoformat(timespec="seconds")
    pending = store.unresolved_predictions(cutoff)
    # only the past, unresolved one; latest snapshot per key
    assert len(pending) == 1
    assert pending[0]["journey_hash"] == "abc"
    assert pending[0]["ts"] == "2026-07-20T05:10:00+00:00"

    store.record_outcome("abc", 0, "arrival", past, past, 11, False, "trip_refetch")
    assert store.unresolved_predictions(cutoff) == []

    hist = store.history(days=30)
    assert len(hist) >= 1
    assert hist[-1]["realized_delay"] == 11
    assert hist[-1]["db_forecast_iris"] == 7

    counts = store.counts()
    assert counts["prediction_log"] == 3
    assert counts["outcome"] == 1
