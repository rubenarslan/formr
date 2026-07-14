"""Resolve realized arrivals/cancellations for logged predictions.

No standing scraper: on travel days we poll only the trains that were actually
shown to the user. Ground-truth preference order:

1. transport.rest trip refetch (vendo world) — realized times appear as the
   prognosed time once the event has happened.
2. IRIS fchg at the stop's station (station world) — last `ct` is the realized
   time; this matches how the Bahn-Vorhersage training data defines actuals.
"""

from __future__ import annotations

import asyncio
import logging
from datetime import UTC, datetime, timedelta

import httpx

from .config import Settings
from .iris import Iris
from .store import Store
from .transportrest import TransportRest

log = logging.getLogger("trainpunct.resolver")


async def resolve_once(settings: Settings, store: Store) -> int:
    """Resolve all overdue predictions; returns the number of outcomes written."""
    cutoff = (datetime.now(UTC) - timedelta(minutes=10)).isoformat(timespec="seconds")
    pending = store.unresolved_predictions(cutoff)
    if not pending:
        return 0

    transport = TransportRest(settings.transportrest_url)
    iris = Iris(settings.iris_url)
    written = 0
    try:
        for row in pending:
            planned = datetime.fromisoformat(row["planned"]) if row["planned"] else None
            if planned is None:
                continue
            overdue_min = (datetime.now(UTC) - planned).total_seconds() / 60
            gave_up = overdue_min > settings.resolve_grace_min + 24 * 60

            outcome = await _try_trip(transport, row, planned)
            if outcome is None:
                outcome = await _try_iris(iris, row, planned)
            if outcome is None:
                if gave_up:
                    store.record_outcome(
                        row["journey_hash"], row["leg_idx"], row["event"],
                        row["planned"], None, None, False, "gave_up",
                    )
                    written += 1
                continue
            realized, delay_min, cancelled, method = outcome
            store.record_outcome(
                row["journey_hash"], row["leg_idx"], row["event"], row["planned"],
                realized.isoformat() if realized else None, delay_min, cancelled, method,
            )
            written += 1
    finally:
        await transport.aclose()
        await iris.aclose()
    return written


async def _try_trip(
    transport: TransportRest, row, planned: datetime
) -> tuple[datetime | None, int | None, bool, str] | None:
    trip_id = row["trip_id"]
    if not trip_id:
        return None
    try:
        leg = await transport.trip(trip_id)
    except httpx.HTTPError:
        return None
    if leg.cancelled:
        return None, None, True, "trip_refetch"
    for so in leg.stopovers:
        match_planned = so.planned_arrival if row["event"] == "arrival" else so.planned_departure
        if match_planned == planned:
            if so.cancelled:
                return None, None, True, "trip_refetch"
            realized = so.arrival if row["event"] == "arrival" else so.departure
            if realized is None:
                return None
            # only trust it once the event is in the past (prognosis has settled)
            if realized > datetime.now(UTC):
                return None
            delay = int((realized - planned).total_seconds() // 60)
            return realized, delay, False, "trip_refetch"
    return None


async def _try_iris(
    iris: Iris, row, planned: datetime
) -> tuple[datetime | None, int | None, bool, str] | None:
    eva = row["station_eva"]
    if not eva or not str(eva).isdigit():
        return None
    number = (row["fahrt_nr"] or "").strip()
    category = (row["line"] or "").split(" ")[0]
    try:
        stop = await iris.stop_state(int(eva), category, number, planned)
    except httpx.HTTPError:
        return None
    if stop is None:
        return None
    ev = stop.arrival if row["event"] == "arrival" else stop.departure
    if ev.cancelled:
        return None, None, True, "iris_fchg"
    realized = ev.ct or ev.pt
    if realized is None or realized > datetime.now(UTC):
        return None
    delay = int((realized - planned).total_seconds() // 60)
    return realized, delay, False, "iris_fchg"


async def resolve_loop(settings: Settings, store: Store) -> None:
    log.info("resolver loop started (every %ss)", settings.resolve_poll_seconds)
    while True:
        try:
            n = await resolve_once(settings, store)
            if n:
                log.info("resolved %d outcomes", n)
        except Exception:  # noqa: BLE001
            log.exception("resolver pass failed")
        await asyncio.sleep(settings.resolve_poll_seconds)
