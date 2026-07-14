"""Client for a db-rest / transport.rest instance (vendo/bahn.de world, FPTF JSON).

Used for journey enumeration (only this side knows connections) and trip status.
Delays here are in seconds; we convert to minutes at the edge.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime

import httpx


@dataclass
class Stop:
    id: str
    name: str
    lat: float | None
    lon: float | None


@dataclass
class Stopover:
    stop: Stop
    planned_arrival: datetime | None
    arrival: datetime | None
    planned_departure: datetime | None
    departure: datetime | None
    cancelled: bool = False


@dataclass
class Leg:
    trip_id: str | None
    walking: bool
    origin: Stop
    destination: Stop
    planned_departure: datetime | None
    departure: datetime | None  # prognosed
    planned_arrival: datetime | None
    arrival: datetime | None  # prognosed
    cancelled: bool
    line_name: str | None = None  # "RE 4"
    fahrt_nr: str | None = None  # "10426"
    product_name: str | None = None  # "RE"
    admin_code: str | None = None
    operator_name: str | None = None
    departure_platform: str | None = None
    arrival_platform: str | None = None
    stopovers: list[Stopover] = field(default_factory=list)
    walk_duration_min: int | None = None
    remarks: list[str] = field(default_factory=list)

    @property
    def departure_delay_min(self) -> int:
        if self.departure and self.planned_departure:
            return int((self.departure - self.planned_departure).total_seconds() // 60)
        return 0

    @property
    def arrival_delay_min(self) -> int:
        if self.arrival and self.planned_arrival:
            return int((self.arrival - self.planned_arrival).total_seconds() // 60)
        return 0


@dataclass
class Journey:
    legs: list[Leg]
    refresh_token: str | None = None

    @property
    def train_legs(self) -> list[Leg]:
        return [leg for leg in self.legs if not leg.walking]


def _dt(value: str | None) -> datetime | None:
    if not value:
        return None
    return datetime.fromisoformat(value)


def _stop(obj: dict | None) -> Stop:
    obj = obj or {}
    loc = obj.get("location") or {}
    return Stop(
        id=str(obj.get("id") or ""),
        name=obj.get("name") or "",
        lat=loc.get("latitude"),
        lon=loc.get("longitude"),
    )


def _stopover(obj: dict) -> Stopover:
    return Stopover(
        stop=_stop(obj.get("stop")),
        planned_arrival=_dt(obj.get("plannedArrival")),
        arrival=_dt(obj.get("arrival")),
        planned_departure=_dt(obj.get("plannedDeparture")),
        departure=_dt(obj.get("departure")),
        cancelled=bool(obj.get("cancelled", False)),
    )


def parse_leg(obj: dict) -> Leg:
    line = obj.get("line") or {}
    walking = bool(obj.get("walking", False))
    walk_duration_min = None
    if walking and obj.get("plannedDeparture") and obj.get("plannedArrival"):
        dep, arr = _dt(obj["plannedDeparture"]), _dt(obj["plannedArrival"])
        walk_duration_min = max(0, int((arr - dep).total_seconds() // 60))
    return Leg(
        trip_id=obj.get("tripId"),
        walking=walking,
        origin=_stop(obj.get("origin")),
        destination=_stop(obj.get("destination")),
        planned_departure=_dt(obj.get("plannedDeparture")),
        departure=_dt(obj.get("departure")),
        planned_arrival=_dt(obj.get("plannedArrival")),
        arrival=_dt(obj.get("arrival")),
        cancelled=bool(obj.get("cancelled", False)),
        line_name=line.get("name"),
        fahrt_nr=str(line.get("fahrtNr")) if line.get("fahrtNr") is not None else None,
        product_name=line.get("productName"),
        admin_code=line.get("adminCode"),
        operator_name=(line.get("operator") or {}).get("name"),
        departure_platform=obj.get("departurePlatform"),
        arrival_platform=obj.get("arrivalPlatform"),
        stopovers=[_stopover(s) for s in obj.get("stopovers") or []],
        walk_duration_min=walk_duration_min,
        remarks=[r.get("text", "") for r in obj.get("remarks") or [] if isinstance(r, dict)],
    )


def parse_journeys(payload: dict) -> list[Journey]:
    journeys = []
    for j in payload.get("journeys") or []:
        journeys.append(
            Journey(
                legs=[parse_leg(leg) for leg in j.get("legs") or []],
                refresh_token=j.get("refreshToken"),
            )
        )
    return journeys


class TransportRest:
    def __init__(self, base_url: str, timeout: float = 25.0):
        self.base_url = base_url.rstrip("/")
        self._client = httpx.AsyncClient(timeout=timeout)

    async def aclose(self) -> None:
        await self._client.aclose()

    async def journeys(
        self,
        from_eva: int,
        to_eva: int,
        departure: datetime | None = None,
        results: int = 8,
    ) -> tuple[list[Journey], dict]:
        params: dict = {
            "from": from_eva,
            "to": to_eva,
            "results": results,
            "stopovers": "true",
            "remarks": "true",
        }
        if departure is not None:
            params["departure"] = departure.isoformat()
        r = await self._client.get(f"{self.base_url}/journeys", params=params)
        r.raise_for_status()
        payload = r.json()
        return parse_journeys(payload), payload

    async def trip(self, trip_id: str) -> Leg:
        r = await self._client.get(
            f"{self.base_url}/trips/{httpx.QueryParams({'id': trip_id})['id']}",
            params={"stopovers": "true"},
        )
        r.raise_for_status()
        return parse_leg(r.json().get("trip") or {})

    async def locations(self, query: str, results: int = 5) -> list[dict]:
        r = await self._client.get(
            f"{self.base_url}/locations",
            params={"query": query, "results": results, "poi": "false", "addresses": "false"},
        )
        r.raise_for_status()
        return r.json()
