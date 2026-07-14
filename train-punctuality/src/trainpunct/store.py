"""SQLite (WAL) storage: journey snapshots, served predictions, realized outcomes."""

from __future__ import annotations

import json
import sqlite3
from datetime import UTC, datetime
from pathlib import Path

SCHEMA = """
CREATE TABLE IF NOT EXISTS journey_snapshot (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  queried_ts TEXT NOT NULL,
  day_kind TEXT,
  journey_hash TEXT NOT NULL,
  raw_json TEXT
);
CREATE INDEX IF NOT EXISTS ix_snapshot_hash ON journey_snapshot(journey_hash, queried_ts);

CREATE TABLE IF NOT EXISTS prediction_log (
  ts TEXT NOT NULL,
  journey_hash TEXT NOT NULL,
  day_kind TEXT,
  leg_idx INTEGER NOT NULL,
  trip_id TEXT,
  fahrt_nr TEXT,
  line TEXT,
  station_eva TEXT,
  event TEXT,                        -- arrival | departure
  planned TEXT,
  db_forecast_vendo INTEGER,         -- minutes
  db_forecast_iris INTEGER,          -- minutes, NULL if IRIS had no data
  q50 INTEGER, q80 INTEGER, q95 INTEGER,
  eff_q50 INTEGER, eff_q80 INTEGER, eff_q95 INTEGER,
  p_miss REAL, p_cancel REAL,
  horizon_min INTEGER,
  model_digest TEXT,
  PRIMARY KEY (ts, journey_hash, leg_idx, event)
);
CREATE INDEX IF NOT EXISTS ix_pred_unresolved ON prediction_log(journey_hash, leg_idx, event);

CREATE TABLE IF NOT EXISTS outcome (
  journey_hash TEXT NOT NULL,
  leg_idx INTEGER NOT NULL,
  event TEXT NOT NULL,
  planned TEXT,
  realized TEXT,
  delay_min INTEGER,
  cancelled INTEGER DEFAULT 0,
  method TEXT,                       -- trip_refetch | iris_fchg | gave_up
  resolved_ts TEXT,
  PRIMARY KEY (journey_hash, leg_idx, event)
);
"""


def utcnow() -> str:
    return datetime.now(UTC).isoformat(timespec="seconds")


class Store:
    def __init__(self, path: str | Path):
        Path(path).parent.mkdir(parents=True, exist_ok=True)
        self.conn = sqlite3.connect(str(path), check_same_thread=False)
        self.conn.execute("PRAGMA journal_mode=WAL")
        self.conn.execute("PRAGMA busy_timeout=10000")
        self.conn.executescript(SCHEMA)
        self.conn.row_factory = sqlite3.Row

    def close(self) -> None:
        self.conn.close()

    # -- writes ---------------------------------------------------------------

    def log_journey_snapshot(self, day_kind: str, journey_hash: str, raw: dict) -> None:
        self.conn.execute(
            "INSERT INTO journey_snapshot (queried_ts, day_kind, journey_hash, raw_json)"
            " VALUES (?,?,?,?)",
            (utcnow(), day_kind, journey_hash, json.dumps(raw)),
        )
        self.conn.commit()

    def log_predictions(self, rows: list[dict]) -> None:
        if not rows:
            return
        cols = [
            "ts", "journey_hash", "day_kind", "leg_idx", "trip_id", "fahrt_nr", "line",
            "station_eva", "event", "planned", "db_forecast_vendo", "db_forecast_iris",
            "q50", "q80", "q95", "eff_q50", "eff_q80", "eff_q95",
            "p_miss", "p_cancel", "horizon_min", "model_digest",
        ]
        placeholders = ",".join("?" * len(cols))
        self.conn.executemany(
            f"INSERT OR REPLACE INTO prediction_log ({','.join(cols)}) VALUES ({placeholders})",
            [tuple(r.get(c) for c in cols) for r in rows],
        )
        self.conn.commit()

    def record_outcome(
        self,
        journey_hash: str,
        leg_idx: int,
        event: str,
        planned: str | None,
        realized: str | None,
        delay_min: int | None,
        cancelled: bool,
        method: str,
    ) -> None:
        self.conn.execute(
            "INSERT OR REPLACE INTO outcome"
            " (journey_hash, leg_idx, event, planned, realized, delay_min, cancelled,"
            "  method, resolved_ts) VALUES (?,?,?,?,?,?,?,?,?)",
            (journey_hash, leg_idx, event, planned, realized, delay_min,
             int(cancelled), method, utcnow()),
        )
        self.conn.commit()

    # -- reads ----------------------------------------------------------------

    def unresolved_predictions(self, older_than_iso: str) -> list[sqlite3.Row]:
        """Latest prediction row per (journey_hash, leg_idx, event) without an outcome,
        whose planned event time is in the past."""
        return self.conn.execute(
            """
            SELECT p.* FROM prediction_log p
            LEFT JOIN outcome o USING (journey_hash, leg_idx, event)
            WHERE o.journey_hash IS NULL AND p.planned IS NOT NULL AND p.planned < ?
            GROUP BY p.journey_hash, p.leg_idx, p.event
            HAVING p.ts = MAX(p.ts)
            """,
            (older_than_iso,),
        ).fetchall()

    def history(self, days: int = 30) -> list[dict]:
        rows = self.conn.execute(
            """
            SELECT p.ts, p.journey_hash, p.leg_idx, p.event, p.line, p.planned,
                   p.horizon_min, p.db_forecast_vendo, p.db_forecast_iris,
                   p.q50, p.q80, p.q95, p.eff_q50, p.eff_q80, p.eff_q95,
                   p.p_miss, p.p_cancel,
                   o.delay_min AS realized_delay, o.cancelled, o.method
            FROM prediction_log p
            JOIN outcome o USING (journey_hash, leg_idx, event)
            WHERE p.ts >= datetime('now', ?)
            ORDER BY p.ts
            """,
            (f"-{int(days)} days",),
        ).fetchall()
        return [dict(r) for r in rows]

    def counts(self) -> dict:
        c = {}
        for table in ("journey_snapshot", "prediction_log", "outcome"):
            c[table] = self.conn.execute(f"SELECT COUNT(*) FROM {table}").fetchone()[0]
        return c
