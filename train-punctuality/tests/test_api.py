import json

import httpx
import respx
from fastapi.testclient import TestClient

from tests.conftest import predictor_response
from trainpunct.api import create_app
from trainpunct.config import Settings, Station

EMPTY_TT = "<timetable/>"


def _settings(tmp_path) -> Settings:
    return Settings(
        origin=Station(eva=8000001, name="Aachen Hbf"),
        destination=Station(eva=8000251, name="Witten Hbf"),
        predictor_url="http://predictor.test",
        transportrest_url="http://tr.test",
        iris_url="http://iris.test",
        db_path=str(tmp_path / "api.sqlite3"),
        cancel_table_path=str(tmp_path / "missing.json"),
    )


def _predictor_callback(request: httpx.Request) -> httpx.Response:
    payload = json.loads(request.content)
    n_rows = len(payload["number"])
    transfer_rows = {
        i: 0.85
        for i, v in enumerate(payload.get("minimal_transfer_time", []))
        if v is not None
    }
    return httpx.Response(200, json=predictor_response(n_rows, transfer_rows=transfer_rows))


@respx.mock(assert_all_called=False)
def test_options_end_to_end(respx_mock, tmp_path, journeys_payload, iris_plan_xml, iris_fchg_xml):
    respx_mock.get(url__regex=r"http://tr\.test/journeys.*").respond(json=journeys_payload)
    respx_mock.get(url__regex=r"http://iris\.test/iris-tt/timetable/plan/8000251/.*").respond(
        text=iris_plan_xml
    )
    respx_mock.get(url__regex=r"http://iris\.test/iris-tt/timetable/plan/\d+/.*").respond(
        text=EMPTY_TT
    )
    respx_mock.get("http://iris.test/iris-tt/timetable/fchg/8000251").respond(text=iris_fchg_xml)
    respx_mock.get(url__regex=r"http://iris\.test/iris-tt/timetable/fchg/\d+").respond(
        text=EMPTY_TT
    )
    respx_mock.post("http://predictor.test/rate-journeys/").mock(side_effect=_predictor_callback)
    respx_mock.get("http://predictor.test/training-stats/").respond(json={})

    settings = _settings(tmp_path)
    app = create_app(settings)
    with TestClient(app) as client:
        r = client.get("/api/options?day=auto")
        assert r.status_code == 200
        body = r.json()

        assert body["model_available"] is True
        assert len(body["options"]) == 2

        direct = next(o for o in body["options"] if len(o["legs"]) == 1)
        via = next(o for o in body["options"] if len(o["legs"]) == 2)

        # IRIS override: fchg says 09:57 vs planned 09:45 -> 12; vendo said 5
        assert direct["legs"][0]["arr"]["iris_delay_min"] == 12
        assert direct["legs"][0]["arr"]["vendo_delay_min"] == 5
        # point-mass predictor at prognosis -> conditional q50 = IRIS delay
        assert direct["model"]["q50"] == 12
        assert direct["model"]["p_miss"] == 0.0
        assert direct["badge"] == "red"  # q50 12 > 10

        # 2-leg option: transfer score 0.85 -> p_miss 0.15; IRIS cancels its Witten arrival
        assert via["cancelled_now"] is True
        assert via["model"]["p_miss"] == 0.15
        assert via["badge"] == "red"

        # ranking: direct option first (earlier effective arrival)
        assert body["options"][0]["journey_hash"] == direct["journey_hash"]

        # predictions were logged for the resolver
        h = client.get("/api/health")
        counts = h.json()["db_counts"]
        assert counts["prediction_log"] == 2
        assert counts["journey_snapshot"] == 2

        # cached second call does not double-log
        client.get("/api/options?day=auto")
        assert client.get("/api/health").json()["db_counts"]["prediction_log"] == 2


@respx.mock(assert_all_called=False)
def test_options_degrades_without_predictor(respx_mock, tmp_path, journeys_payload):
    respx_mock.get(url__regex=r"http://tr\.test/journeys.*").respond(json=journeys_payload)
    respx_mock.get(url__regex=r"http://iris\.test/.*").respond(text=EMPTY_TT)
    respx_mock.post("http://predictor.test/rate-journeys/").respond(status_code=503)

    app = create_app(_settings(tmp_path))
    with TestClient(app) as client:
        r = client.get("/api/options?day=auto")
        assert r.status_code == 200
        body = r.json()
        assert body["model_available"] is False
        assert body["options"][0]["model"] is None
