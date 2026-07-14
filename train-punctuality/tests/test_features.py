from datetime import UTC, datetime

from trainpunct.features import (
    PAYLOAD_COLUMNS,
    TRANSFER_COLUMNS,
    bearing_deg,
    build_payload,
    haversine_m,
)
from trainpunct.transportrest import parse_journeys

NOW = datetime(2026, 7, 20, 5, 0, tzinfo=UTC)  # 07:00 local, before both departures


def _payload(journeys_payload, **kwargs):
    journeys = parse_journeys(journeys_payload)
    return build_payload(journeys, now=NOW, **kwargs)


def test_row_count_and_order(journeys_payload):
    payload = _payload(journeys_payload)
    # journey 1: 1 leg -> 2 rows; journey 2: 2 legs -> 4 rows
    for col in PAYLOAD_COLUMNS + TRANSFER_COLUMNS:
        assert len(payload.columns[col]) == 6, col
    assert payload.columns["is_arrival"] == [False, True, False, True, False, True]
    refs = payload.refs
    assert [r.event for r in refs] == ["departure", "arrival"] * 3
    assert refs[0].eva == "8000001" and refs[1].eva == "8000251"
    assert refs[2].eva == "8000001" and refs[3].eva == "8000207"


def test_basic_feature_values(journeys_payload):
    payload = _payload(journeys_payload)
    cols = payload.columns
    # RE4 departure Aachen
    assert cols["number"][0] == 10426
    assert cols["category"][0] == "RE"
    assert cols["line"][0] == "4"  # "RE 4" stripped of category
    assert cols["operator"][0] == "NXG4"
    assert cols["is_regional"][0] is True
    assert cols["weekday"][0] == 1  # 2026-07-20 is a Monday, ISO
    assert cols["minute_of_day"][0] == 7 * 60 + 30
    assert cols["stop_sequence"][0] == 0
    assert cols["distance_traveled"][0] == 0
    # RE4 arrival Witten: vendo prognosis +5
    assert cols["delay_prognosed"][1] == 5
    assert cols["stop_sequence"][1] == 5
    # distance Aachen->Witten chain must exceed straight line
    straight = haversine_m(50.7678, 6.0911, 51.4366, 7.3296)
    assert cols["distance_traveled"][1] > straight
    # dwell at Witten: 09:45 arr / (dep not in leg-truncated stopovers) -> None ok, or int
    assert cols["dwell_time_schedule"][0] is None  # first stop has no arrival
    # minutes_to_prognosed_time: 07:32 local dep vs 07:00 now -> 32
    assert cols["minutes_to_prognosed_time"][0] == 32


def test_bearing_matches_reference_formula(journeys_payload):
    payload = _payload(journeys_payload)
    expected = bearing_deg(50.7678, 6.0911, 51.4366, 7.3296)
    assert payload.columns["bearing"][0] == expected
    assert 40 <= expected <= 60  # north-east


def test_transfer_columns_pattern(journeys_payload):
    payload = _payload(journeys_payload, min_transfer_overrides={8000207: 8})
    pt = payload.columns["prognosed_transfer_time"]
    mt = payload.columns["minimal_transfer_time"]
    # single leg journey: no transfers
    assert pt[0] is None and pt[1] is None
    # 2-leg journey: dep A None, arr B & dep B share the transfer, arr C None
    assert pt[2] is None and pt[5] is None
    assert pt[3] == pt[4] == 13  # 08:35 -> 08:48
    assert mt[3] == mt[4] == 8  # Köln override
    assert mt[2] is None and mt[5] is None


def test_iris_override_wins(journeys_payload):
    overrides = {("10426", "8000251", "arrival"): (12, False)}
    payload = _payload(journeys_payload, overrides=overrides)
    assert payload.columns["delay_prognosed"][1] == 12
    assert payload.refs[1].delay_source == "iris"
    assert payload.refs[0].delay_source == "vendo"


def test_iris_cancellation_flag_propagates(journeys_payload):
    overrides = {("10426", "8000251", "arrival"): (0, True)}
    payload = _payload(journeys_payload, overrides=overrides)
    assert payload.refs[1].cancelled is True
