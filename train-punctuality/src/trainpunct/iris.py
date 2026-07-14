"""Keyless client for the DB InfraGO IRIS timetable feed (station world).

This is the feed the Bahn-Vorhersage model was trained on, and it is sometimes
out of sync with the vendo/bahn.de forecasts. We query it on demand for the
boarding/transfer stations of the current options only — no standing scraper.

XML endpoints:
  /iris-tt/timetable/plan/{eva}/{yymmdd}/{hh}  planned stops for one hour
  /iris-tt/timetable/fchg/{eva}                full change set (forecasts ct,
                                               cancellations cs='c'/clt, platforms)
Times are local (Europe/Berlin), format YYMMDDHHMM.
"""

from __future__ import annotations

import time
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from datetime import datetime
from zoneinfo import ZoneInfo

import httpx

BERLIN = ZoneInfo("Europe/Berlin")


@dataclass
class IrisEvent:
    pt: datetime | None = None  # planned
    ct: datetime | None = None  # current forecast
    cancelled: bool = False


@dataclass
class IrisStop:
    stop_id: str
    category: str  # e.g. "RE", "ICE"
    number: str  # train number, e.g. "10426"
    arrival: IrisEvent
    departure: IrisEvent

    def delay_min(self, event: str) -> int | None:
        ev = self.arrival if event == "arrival" else self.departure
        if ev.ct and ev.pt:
            return int((ev.ct - ev.pt).total_seconds() // 60)
        if ev.pt:
            return 0  # no change published -> on time as far as IRIS knows
        return None


def _parse_time(value: str | None) -> datetime | None:
    if not value:
        return None
    return datetime.strptime(value, "%y%m%d%H%M").replace(tzinfo=BERLIN)


def _event(el: ET.Element | None) -> IrisEvent:
    if el is None:
        return IrisEvent()
    return IrisEvent(
        pt=_parse_time(el.get("pt")),
        ct=_parse_time(el.get("ct")),
        cancelled=el.get("cs") == "c" or el.get("clt") is not None,
    )


def parse_timetable(xml_text: str) -> dict[str, IrisStop]:
    """Parse a plan or fchg document into {stop_id: IrisStop}."""
    root = ET.fromstring(xml_text)
    stops: dict[str, IrisStop] = {}
    for s in root.iter("s"):
        tl = s.find("tl")
        stops[s.get("id", "")] = IrisStop(
            stop_id=s.get("id", ""),
            category=tl.get("c", "") if tl is not None else "",
            number=tl.get("n", "") if tl is not None else "",
            arrival=_event(s.find("ar")),
            departure=_event(s.find("dp")),
        )
    return stops


def merge_change(plan: IrisStop, change: IrisStop) -> IrisStop:
    for attr in ("arrival", "departure"):
        pl_ev: IrisEvent = getattr(plan, attr)
        ch_ev: IrisEvent = getattr(change, attr)
        if ch_ev.ct:
            pl_ev.ct = ch_ev.ct
        if ch_ev.cancelled:
            pl_ev.cancelled = True
    return plan


class Iris:
    def __init__(
        self,
        base_url: str,
        timeout: float = 15.0,
        fchg_ttl: float = 60.0,
        plan_ttl: float = 900.0,
    ):
        self.base_url = base_url.rstrip("/")
        self._client = httpx.AsyncClient(timeout=timeout)
        self._fchg_cache: dict[int, tuple[float, dict[str, IrisStop]]] = {}
        self._fchg_ttl = fchg_ttl
        self._plan_cache: dict[str, tuple[float, dict[str, IrisStop]]] = {}
        self._plan_ttl = plan_ttl

    async def aclose(self) -> None:
        await self._client.aclose()

    async def _get(self, path: str) -> str:
        r = await self._client.get(f"{self.base_url}{path}")
        r.raise_for_status()
        return r.text

    async def plan(self, eva: int, when_local: datetime) -> dict[str, IrisStop]:
        when = when_local.astimezone(BERLIN)
        path = f"/iris-tt/timetable/plan/{eva}/{when.strftime('%y%m%d')}/{when.strftime('%H')}"
        now = time.monotonic()
        cached = self._plan_cache.get(path)
        if cached and now - cached[0] < self._plan_ttl:
            stops = cached[1]
        else:
            stops = parse_timetable(await self._get(path))
            self._plan_cache[path] = (now, stops)
        # callers merge fchg into the result; hand out copies so the cache stays pristine
        return {k: IrisStop(v.stop_id, v.category, v.number,
                            IrisEvent(v.arrival.pt, v.arrival.ct, v.arrival.cancelled),
                            IrisEvent(v.departure.pt, v.departure.ct, v.departure.cancelled))
                for k, v in stops.items()}

    async def fchg(self, eva: int) -> dict[str, IrisStop]:
        now = time.monotonic()
        cached = self._fchg_cache.get(eva)
        if cached and now - cached[0] < self._fchg_ttl:
            return cached[1]
        stops = parse_timetable(await self._get(f"/iris-tt/timetable/fchg/{eva}"))
        self._fchg_cache[eva] = (now, stops)
        return stops

    async def stop_state(
        self, eva: int, category: str, number: str, planned_local: datetime
    ) -> IrisStop | None:
        """Find one train's IRIS state at a station: plan for the planned hour + fchg overlay.

        Returns None when the stop is not in the plan (IRIS occasionally lags or the
        train does not stop there in the station world).
        """
        try:
            plan = await self.plan(eva, planned_local)
        except (httpx.HTTPError, ET.ParseError):
            return None
        match = None
        for stop in plan.values():
            if stop.number == str(number) and (not category or stop.category == category):
                match = stop
                break
        if match is None:
            return None
        try:
            changes = await self.fchg(eva)
        except (httpx.HTTPError, ET.ParseError):
            return match
        change = changes.get(match.stop_id)
        return merge_change(match, change) if change else match
