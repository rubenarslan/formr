"""Journey legs -> Bahn-Vorhersage predictor payload.

Mirrors the reference mapping in bahnvorhersage's webserver/journeys.py (GPLv3),
with three deliberate differences:

- distance_traveled: haversine chain over the leg's stopovers (they use their
  internal rail graph; the chain is a close lower bound).
- weekday: ISO 1-7 as the model was trained (polars dt.weekday()); their own
  frontend sends Python's 0-6 — an off-by-one we do not reproduce.
- delay_prognosed: IRIS (station world) value first when available, since the
  model was trained on IRIS data; vendo value as fallback.

Payload semantics verified against predictor_webserver/__init__.py docstrings:
predictions[j] = P(actual delay == delay_prognosed + (j - offset)), offset = 3,
34 classes, so the deviation support is [-3, +30] with the last bin ≈ ">= +30".
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from datetime import UTC, datetime

from .transportrest import Journey, Leg

# From bahnvorhersage public_config.py (GPLv3).
LONG_DISTANCE_CATEGORIES = {
    "IC", "EC", "ECE", "ICE", "EN", "RJ", "RJX", "TGV", "FLX", "NJ",
    "THA", "NEX", "EIC", "UEX", "EST", "WB",
}

PAYLOAD_COLUMNS = [
    "number", "lat", "lon", "stop_sequence", "distance_traveled",
    "dwell_time_schedule", "dwell_time_prognosed", "bearing", "delay_prognosed",
    "minute_of_day", "minutes_to_prognosed_time", "weekday", "is_regional",
    "is_arrival", "operator", "category", "line",
]
TRANSFER_COLUMNS = ["prognosed_transfer_time", "minimal_transfer_time"]


def haversine_m(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    r = 6371000.0
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp, dl = math.radians(lat2 - lat1), math.radians(lon2 - lon1)
    a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * r * math.asin(math.sqrt(a))


def bearing_deg(lat1: float, lon1: float, lat2: float, lon2: float) -> int:
    """Initial bearing start->end, same formula/rounding as the reference (may be negative)."""
    dl = math.radians(lon2 - lon1)
    y = math.sin(dl) * math.cos(math.radians(lat2))
    x = math.cos(math.radians(lat1)) * math.sin(math.radians(lat2)) - math.sin(
        math.radians(lat1)
    ) * math.cos(math.radians(lat2)) * math.cos(dl)
    return round(math.degrees(math.atan2(y, x)))


def _line_short(line_name: str | None, category: str | None) -> str:
    """'RE 4' -> '4' when prefixed by the category, else the raw name (reference behavior)."""
    if not line_name:
        return ""
    if category and line_name.upper().startswith(category.upper()):
        stripped = line_name[len(category):].strip()
        if stripped:
            return stripped
    return line_name


def _minutes(td_seconds: float) -> int:
    return int(td_seconds // 60)


@dataclass
class RowRef:
    """Maps a payload row back to its journey/leg/event for post-processing."""

    journey_idx: int
    leg_idx: int  # index within journey.train_legs
    event: str  # "departure" | "arrival"
    eva: str
    planned: datetime | None
    prognosed: datetime | None
    delay_prognosed: int
    delay_source: str  # "iris" | "vendo"
    cancelled: bool


@dataclass
class PredictorPayload:
    columns: dict[str, list] = field(default_factory=dict)
    refs: list[RowRef] = field(default_factory=list)

    def as_json(self, with_transfer: bool = True) -> dict:
        cols = dict(self.columns)
        if not with_transfer:
            for c in TRANSFER_COLUMNS:
                cols.pop(c, None)
        return cols


DelayOverride = dict[tuple[str, str, str], tuple[int, bool]]
"""(fahrt_nr, eva, event) -> (delay_min from IRIS, cancelled per IRIS)"""


def _stopover_index(leg: Leg, stop_id: str, name: str) -> int:
    for i, so in enumerate(leg.stopovers):
        if so.stop.id == stop_id:
            return i
    for i, so in enumerate(leg.stopovers):
        if so.stop.name == name:
            return i
    return 0


def _distance_chain_m(leg: Leg, upto: int) -> int:
    total = 0.0
    prev = None
    for so in leg.stopovers[: upto + 1]:
        if so.stop.lat is None or so.stop.lon is None:
            continue
        if prev is not None:
            total += haversine_m(prev.lat, prev.lon, so.stop.lat, so.stop.lon)
        prev = so.stop
    return int(total)


def _dwell_minutes(leg: Leg, idx: int, planned: bool) -> int | None:
    if idx >= len(leg.stopovers):
        return None
    so = leg.stopovers[idx]
    arr = so.planned_arrival if planned else so.arrival
    dep = so.planned_departure if planned else so.departure
    if arr is None or dep is None:
        return None
    return max(0, _minutes((dep - arr).total_seconds()))


def build_payload(
    journeys: list[Journey],
    overrides: DelayOverride | None = None,
    min_transfer_default: int = 5,
    min_transfer_overrides: dict[int, int] | None = None,
    now: datetime | None = None,
) -> PredictorPayload:
    """Build the dict-of-columns payload: per train leg one departure and one arrival row."""
    overrides = overrides or {}
    min_transfer_overrides = min_transfer_overrides or {}
    now = now or datetime.now(UTC)

    payload = PredictorPayload(columns={c: [] for c in PAYLOAD_COLUMNS + TRANSFER_COLUMNS})
    cols = payload.columns

    for j_idx, journey in enumerate(journeys):
        train_legs = journey.train_legs
        # minimal transfer minutes between consecutive train legs
        walk_between: dict[int, int] = {}
        t_i = -1
        for leg in journey.legs:
            if leg.walking:
                if 0 <= t_i and leg.walk_duration_min is not None:
                    walk_between[t_i] = max(1, leg.walk_duration_min)
            else:
                t_i += 1

        for l_idx, leg in enumerate(train_legs):
            first_stopover = leg.stopovers[0] if leg.stopovers else None
            last_stopover = leg.stopovers[-1] if leg.stopovers else None
            if (
                first_stopover
                and last_stopover
                and first_stopover.stop.lat is not None
                and last_stopover.stop.lat is not None
            ):
                brg = bearing_deg(
                    first_stopover.stop.lat,
                    first_stopover.stop.lon,
                    last_stopover.stop.lat,
                    last_stopover.stop.lon,
                )
            else:
                brg = 0
            is_regional = (leg.product_name or "").upper() not in LONG_DISTANCE_CATEGORIES
            operator = (leg.admin_code or "").replace("_", "")
            category = leg.product_name or ""
            line = _line_short(leg.line_name, category)
            number = int(leg.fahrt_nr) if leg.fahrt_nr and leg.fahrt_nr.isdigit() else 0

            for event in ("departure", "arrival"):
                if event == "departure":
                    stop, planned, prognosed = leg.origin, leg.planned_departure, leg.departure
                    vendo_delay = leg.departure_delay_min
                else:
                    stop, planned, prognosed = leg.destination, leg.planned_arrival, leg.arrival
                    vendo_delay = leg.arrival_delay_min

                key = (str(leg.fahrt_nr), stop.id, event)
                if key in overrides:
                    delay, iris_cancelled = overrides[key]
                    source = "iris"
                else:
                    delay, iris_cancelled = vendo_delay, False
                    source = "vendo"

                seq = _stopover_index(leg, stop.id, stop.name)
                ref_time = prognosed or planned
                minutes_to = (
                    max(0, _minutes((ref_time - now).total_seconds())) if ref_time else 0
                )

                cols["number"].append(number)
                cols["lat"].append(stop.lat if stop.lat is not None else 0.0)
                cols["lon"].append(stop.lon if stop.lon is not None else 0.0)
                cols["stop_sequence"].append(seq)
                cols["distance_traveled"].append(_distance_chain_m(leg, seq))
                cols["dwell_time_schedule"].append(_dwell_minutes(leg, seq, planned=True))
                cols["dwell_time_prognosed"].append(_dwell_minutes(leg, seq, planned=False))
                cols["bearing"].append(brg)
                cols["delay_prognosed"].append(delay)
                cols["minute_of_day"].append(
                    planned.hour * 60 + planned.minute if planned else 0
                )
                cols["minutes_to_prognosed_time"].append(minutes_to)
                cols["weekday"].append(planned.isoweekday() if planned else 1)
                cols["is_regional"].append(is_regional)
                cols["is_arrival"].append(event == "arrival")
                cols["operator"].append(operator)
                cols["category"].append(category)
                cols["line"].append(line)

                payload.refs.append(
                    RowRef(
                        journey_idx=j_idx,
                        leg_idx=l_idx,
                        event=event,
                        eva=stop.id,
                        planned=planned,
                        prognosed=prognosed,
                        delay_prognosed=delay,
                        delay_source=source,
                        cancelled=leg.cancelled or iris_cancelled,
                    )
                )

        # transfer columns: rows are [dep A, arr B, dep B, arr C, ...] per journey
        n_legs = len(train_legs)
        for l_idx, leg in enumerate(train_legs):
            if l_idx == 0:
                dep_transfer = (None, None)
            else:
                dep_transfer = _transfer_times(
                    train_legs[l_idx - 1], leg, walk_between.get(l_idx - 1),
                    min_transfer_default, min_transfer_overrides,
                )
            if l_idx == n_legs - 1:
                arr_transfer = (None, None)
            else:
                arr_transfer = _transfer_times(
                    leg, train_legs[l_idx + 1], walk_between.get(l_idx),
                    min_transfer_default, min_transfer_overrides,
                )
            cols["prognosed_transfer_time"].extend([dep_transfer[0], arr_transfer[0]])
            cols["minimal_transfer_time"].extend([dep_transfer[1], arr_transfer[1]])

    return payload


def _transfer_times(
    arriving: Leg,
    departing: Leg,
    walk_minutes: int | None,
    default_min: int,
    overrides: dict[int, int],
) -> tuple[int | None, int | None]:
    if arriving.arrival is None or departing.departure is None:
        return None, None
    prognosed = _minutes((departing.departure - arriving.arrival).total_seconds())
    if walk_minutes is not None:
        minimal = walk_minutes
    else:
        eva = int(arriving.destination.id) if arriving.destination.id.isdigit() else 0
        minimal = overrides.get(eva, default_min)
    return prognosed, minimal
