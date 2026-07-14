"""FastAPI app: /api/options, /api/health, /api/history, /api/calibration + PWA static files."""

from __future__ import annotations

import asyncio
import hashlib
import logging
import time
from datetime import UTC, datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import httpx
from fastapi import FastAPI, Query
from fastapi.responses import JSONResponse
from fastapi.staticfiles import StaticFiles

from . import calibration
from .cancellations import CancelTable
from .config import Settings, load_settings
from .features import DelayOverride, build_payload
from .iris import Iris
from .predictor_client import Predictor
from .quantiles import OptionInput, badge, compose_options, pmf_from_prediction
from .store import Store
from .transportrest import Journey, TransportRest

log = logging.getLogger("trainpunct")
BERLIN = ZoneInfo("Europe/Berlin")


def journey_hash(journey: Journey) -> str:
    parts = [
        f"{leg.fahrt_nr}@{leg.planned_departure.isoformat() if leg.planned_departure else '?'}"
        for leg in journey.train_legs
    ]
    return hashlib.sha1("|".join(parts).encode()).hexdigest()[:16]


def pick_day_kind(settings: Settings, day: str | None, now: datetime) -> tuple[str, datetime]:
    """Resolve requested day to (day_kind, departure search start, local)."""
    local = now.astimezone(BERLIN)
    if day in (None, "", "auto"):
        for kind, w in settings.windows.items():
            if local.isoweekday() == w.day and w.start <= local.strftime("%H:%M") <= w.end:
                return kind, local
        return "adhoc", local
    for kind, w in settings.windows.items():
        if day == kind or (day == "mon" and w.day == 1) or (day == "tue" and w.day == 2):
            days_ahead = (w.day - local.isoweekday()) % 7
            target = (local + timedelta(days=days_ahead)).replace(
                hour=int(w.start[:2]), minute=int(w.start[3:]), second=0, microsecond=0
            )
            if days_ahead == 0 and local.strftime("%H:%M") > w.start:
                target = local if local.strftime("%H:%M") <= w.end else target + timedelta(days=7)
            return kind, target
    return "adhoc", local


def create_app(settings: Settings | None = None) -> FastAPI:
    settings = settings or load_settings()
    app = FastAPI(title="trainpunct")

    state: dict = {"cache": {}, "started": time.time()}

    @app.on_event("startup")
    async def startup() -> None:
        state["store"] = Store(settings.db_path)
        state["transport"] = TransportRest(settings.transportrest_url)
        state["iris"] = Iris(settings.iris_url)
        state["predictor"] = Predictor(settings.predictor_url)
        state["cancel_table"] = CancelTable.load(
            settings.cancel_table_path, settings.cancel_default_rate
        )

    @app.on_event("shutdown")
    async def shutdown() -> None:
        for key in ("transport", "iris", "predictor"):
            client = state.get(key)
            if client:
                await client.aclose()
        if state.get("store"):
            state["store"].close()

    async def gather_iris_overrides(journeys: list[Journey]) -> DelayOverride:
        """IRIS (station world) forecast per unique (train, station, event) — on demand only."""
        iris: Iris = state["iris"]
        wanted: dict[tuple[str, str, str], tuple[int, str, str, datetime]] = {}
        for j in journeys:
            for leg in j.train_legs:
                for event, stop, planned in (
                    ("departure", leg.origin, leg.planned_departure),
                    ("arrival", leg.destination, leg.planned_arrival),
                ):
                    if planned is None or not stop.id.isdigit() or not leg.fahrt_nr:
                        continue
                    key = (str(leg.fahrt_nr), stop.id, event)
                    wanted.setdefault(
                        key, (int(stop.id), leg.product_name or "", leg.fahrt_nr, planned)
                    )

        async def fetch(key, spec):
            eva, category, number, planned = spec
            try:
                stop_state = await iris.stop_state(eva, category, number, planned)
            except Exception:  # noqa: BLE001 - degraded mode is fine
                return key, None
            return key, stop_state

        overrides: DelayOverride = {}
        results = await asyncio.gather(*(fetch(k, s) for k, s in wanted.items()))
        for key, stop_state in results:
            if stop_state is None:
                continue
            event = key[2]
            delay = stop_state.delay_min(event)
            ev = stop_state.arrival if event == "arrival" else stop_state.departure
            if delay is not None:
                overrides[key] = (delay, ev.cancelled)
        return overrides

    @app.get("/api/options")
    async def options(day: str | None = Query(default="auto")) -> JSONResponse:
        now = datetime.now(UTC)
        day_kind, search_start = pick_day_kind(settings, day, now)

        cache_key = day_kind
        cached = state["cache"].get(cache_key)
        if cached and time.time() - cached[0] < settings.options_cache_seconds:
            return JSONResponse(cached[1])

        transport: TransportRest = state["transport"]
        predictor: Predictor = state["predictor"]
        store: Store = state["store"]
        cancel_table: CancelTable = state["cancel_table"]

        direction = settings.windows.get(day_kind)
        if direction and direction.direction == "return":
            from_eva, to_eva = settings.destination.eva, settings.origin.eva
        else:
            from_eva, to_eva = settings.origin.eva, settings.destination.eva

        try:
            journeys, raw = await transport.journeys(
                from_eva, to_eva, departure=search_start, results=settings.journeys_results
            )
        except httpx.HTTPError as e:
            return JSONResponse(
                {"error": f"journey search unavailable: {e}", "generated_at": now.isoformat()},
                status_code=503,
            )
        journeys = [j for j in journeys if j.train_legs]
        journeys.sort(key=lambda j: j.train_legs[0].planned_departure or now)
        if not journeys:
            return JSONResponse({"options": [], "generated_at": now.isoformat()})

        overrides = await gather_iris_overrides(journeys)
        payload = build_payload(
            journeys,
            overrides=overrides,
            min_transfer_default=settings.min_transfer_default,
            min_transfer_overrides=settings.min_transfer_overrides,
            now=now,
        )

        model_ok, prediction = True, None
        try:
            prediction = await predictor.rate_journeys(payload.as_json())
        except httpx.HTTPError as e:
            log.warning("predictor unavailable: %s", e)
            model_ok = False

        # Group payload rows by journey
        rows_by_journey: dict[int, list[int]] = {}
        for i, ref in enumerate(payload.refs):
            rows_by_journey.setdefault(ref.journey_idx, []).append(i)

        anchor = journeys[0].train_legs[-1].planned_arrival or now
        option_inputs: list[OptionInput] = []
        for j_idx, journey in enumerate(journeys):
            legs = journey.train_legs
            final_arr_row = next(
                i
                for i in rows_by_journey[j_idx]
                if payload.refs[i].leg_idx == len(legs) - 1
                and payload.refs[i].event == "arrival"
            )
            ref = payload.refs[final_arr_row]
            if model_ok and prediction:
                pmf = pmf_from_prediction(
                    prediction.predictions[final_arr_row],
                    prediction.offset,
                    ref.delay_prognosed,
                )
            else:
                from .quantiles import Pmf

                pmf = Pmf(ref.delay_prognosed, [1.0])

            p_all_made = 1.0
            if model_ok and prediction:
                for i in rows_by_journey[j_idx]:
                    r = payload.refs[i]
                    if r.event == "arrival" and r.leg_idx < len(legs) - 1:
                        score = prediction.transfer_scores[i]
                        if score is not None:
                            p_all_made *= max(0.0, min(1.0, score))
            p_cancel = cancel_table.p_journey_cancel(
                [(leg.line_name, leg.product_name, leg.planned_departure) for leg in legs]
            )
            cancelled_now = any(
                leg.cancelled or payload.refs[i].cancelled
                for leg in legs
                for i in rows_by_journey[j_idx]
            )
            planned_arr = legs[-1].planned_arrival or now
            option_inputs.append(
                OptionInput(
                    planned_arrival_min=int((planned_arr - anchor).total_seconds() // 60),
                    conditional=pmf,
                    p_miss=1.0 - p_all_made,
                    p_cancel=p_cancel,
                    cancelled_now=cancelled_now,
                )
            )

        results = compose_options(option_inputs, settings.fallback_headway_min)

        options_json, pred_rows = [], []
        ts = now.isoformat(timespec="seconds")
        for j_idx, (journey, opt, res) in enumerate(
            zip(journeys, option_inputs, results, strict=True)
        ):
            legs = journey.train_legs
            jhash = journey_hash(journey)
            first_dep = legs[0].planned_departure
            horizon = int((first_dep - now).total_seconds() // 60) if first_dep else None

            legs_json = []
            for leg in legs:
                iris_dep = overrides.get((str(leg.fahrt_nr), leg.origin.id, "departure"))
                iris_arr = overrides.get((str(leg.fahrt_nr), leg.destination.id, "arrival"))
                legs_json.append(
                    {
                        "line": leg.line_name,
                        "fahrt_nr": leg.fahrt_nr,
                        "dep": {
                            "station": leg.origin.name,
                            "planned": _iso(leg.planned_departure),
                            "vendo_delay_min": leg.departure_delay_min,
                            "iris_delay_min": iris_dep[0] if iris_dep else None,
                            "platform": leg.departure_platform,
                        },
                        "arr": {
                            "station": leg.destination.name,
                            "planned": _iso(leg.planned_arrival),
                            "vendo_delay_min": leg.arrival_delay_min,
                            "iris_delay_min": iris_arr[0] if iris_arr else None,
                            "platform": leg.arrival_platform,
                        },
                        "cancelled": leg.cancelled
                        or (iris_dep[1] if iris_dep else False)
                        or (iris_arr[1] if iris_arr else False),
                    }
                )

            final_leg = legs[-1]
            b = settings.badge
            option_badge = badge(
                res.effective_q.get(50, 0),
                res.effective_q.get(80, 0),
                res.p_fail,
                opt.cancelled_now,
                b.green_q80_max_delay,
                b.green_p_miss_max,
                b.red_q50_min_delay,
                b.red_p_miss_min,
            )
            options_json.append(
                {
                    "journey_hash": jhash,
                    "legs": legs_json,
                    "scheduled_dep": _iso(first_dep),
                    "scheduled_arr": _iso(final_leg.planned_arrival),
                    "db_forecast_arr_delay_min": final_leg.arrival_delay_min,
                    "model": (
                        {
                            "q50": res.conditional_q.get(50),
                            "q80": res.conditional_q.get(80),
                            "q95": res.conditional_q.get(95),
                            "eff_q50": res.effective_q.get(50),
                            "eff_q80": res.effective_q.get(80),
                            "eff_q95": res.effective_q.get(95),
                            "tail_open": res.tail_open,
                            "p_miss": round(res.p_miss, 3),
                            "p_cancel": round(res.p_cancel, 3),
                            "p_fail": round(res.p_fail, 3),
                        }
                        if model_ok
                        else None
                    ),
                    "cancelled_now": opt.cancelled_now,
                    "badge": option_badge,
                    "bahnvorhersage_url": "https://bahnvorhersage.de/",
                }
            )

            store.log_journey_snapshot(day_kind, jhash, {"legs": len(legs)})
            final_ref = None
            for i in rows_by_journey[j_idx]:
                r = payload.refs[i]
                if r.leg_idx == len(legs) - 1 and r.event == "arrival":
                    final_ref = r
            pred_rows.append(
                {
                    "ts": ts,
                    "journey_hash": jhash,
                    "day_kind": day_kind,
                    "leg_idx": len(legs) - 1,
                    "trip_id": final_leg.trip_id,
                    "fahrt_nr": final_leg.fahrt_nr,
                    "line": final_leg.line_name,
                    "station_eva": final_leg.destination.id,
                    "event": "arrival",
                    "planned": _iso(final_leg.planned_arrival),
                    "db_forecast_vendo": final_leg.arrival_delay_min,
                    "db_forecast_iris": (
                        final_ref.delay_prognosed
                        if final_ref and final_ref.delay_source == "iris"
                        else None
                    ),
                    "q50": res.conditional_q.get(50),
                    "q80": res.conditional_q.get(80),
                    "q95": res.conditional_q.get(95),
                    "eff_q50": res.effective_q.get(50),
                    "eff_q80": res.effective_q.get(80),
                    "eff_q95": res.effective_q.get(95),
                    "p_miss": round(res.p_miss, 4),
                    "p_cancel": round(res.p_cancel, 4),
                    "horizon_min": horizon,
                    "model_digest": "bahnvorhersage-predictor:latest",
                }
            )

        store.log_predictions(pred_rows)
        ranked = sorted(
            range(len(options_json)),
            key=lambda i: (
                option_inputs[i].planned_arrival_min + options_json[i]["model"]["eff_q80"]
                if options_json[i]["model"]
                else option_inputs[i].planned_arrival_min,
            ),
        )
        body = {
            "generated_at": now.isoformat(),
            "day_kind": day_kind,
            "model_available": model_ok,
            "desync_threshold_min": settings.desync_show_threshold_min,
            "options": [options_json[i] | {"rank": rank} for rank, i in enumerate(ranked)],
        }
        state["cache"][cache_key] = (time.time(), body)
        return JSONResponse(body)

    @app.get("/api/health")
    async def health() -> JSONResponse:
        predictor: Predictor = state["predictor"]
        ok = await predictor.healthy()
        counts = state["store"].counts() if state.get("store") else {}
        status = 200 if ok else 503
        return JSONResponse(
            {
                "predictor_ok": ok,
                "uptime_s": int(time.time() - state["started"]),
                "db_counts": counts,
            },
            status_code=status,
        )

    @app.get("/api/history")
    async def history(days: int = 30) -> JSONResponse:
        return JSONResponse({"rows": state["store"].history(days=days)})

    @app.get("/api/calibration")
    async def calibration_endpoint(days: int = 90) -> JSONResponse:
        return JSONResponse(calibration.summarize(state["store"].history(days=days)))

    import os

    candidates = [
        os.environ.get("TRAINPUNCT_PWA_DIR"),
        Path.cwd() / "pwa",  # docker WORKDIR /app
        Path(__file__).resolve().parent.parent.parent / "pwa",  # editable install
    ]
    for candidate in candidates:
        if candidate and Path(candidate).is_dir():
            app.mount("/", StaticFiles(directory=str(candidate), html=True), name="pwa")
            break

    return app


def _iso(dt: datetime | None) -> str | None:
    return dt.isoformat() if dt else None
