from datetime import datetime

from trainpunct.iris import BERLIN, merge_change, parse_timetable


def test_parse_plan(iris_plan_xml):
    stops = parse_timetable(iris_plan_xml)
    assert len(stops) == 2
    re4 = stops["1234567890-2607200940-12"]
    assert re4.category == "RE" and re4.number == "10426"
    assert re4.arrival.pt == datetime(2026, 7, 20, 9, 45, tzinfo=BERLIN)
    assert re4.arrival.ct is None
    assert re4.delay_min("arrival") == 0  # no change published


def test_merge_fchg_delay_and_cancellation(iris_plan_xml, iris_fchg_xml):
    plan = parse_timetable(iris_plan_xml)
    changes = parse_timetable(iris_fchg_xml)

    re4 = merge_change(plan["1234567890-2607200940-12"], changes["1234567890-2607200940-12"])
    assert re4.delay_min("arrival") == 12  # 09:45 -> 09:57
    assert re4.arrival.cancelled is False

    re7 = merge_change(plan["9876543210-2607200950-3"], changes["9876543210-2607200950-3"])
    assert re7.arrival.cancelled is True
