from trainpunct.calibration import pinball, summarize


def _row(realized, q50, q80, q95, horizon=30, vendo=0, iris=None, cancelled=0, p_cancel=0.02):
    return {
        "realized_delay": realized, "q50": q50, "q80": q80, "q95": q95,
        "horizon_min": horizon, "db_forecast_vendo": vendo, "db_forecast_iris": iris,
        "cancelled": cancelled, "p_cancel": p_cancel,
    }


def test_pinball():
    assert pinball(10, 5, 0.8) == 4.0  # under-prediction penalized by q
    assert abs(pinball(5, 10, 0.8) - 1.0) < 1e-9  # over-prediction by (1-q)


def test_summarize_coverage_and_bias():
    rows = [
        _row(realized=0, q50=1, q80=4, q95=10),
        _row(realized=5, q50=4, q80=6, q95=12),
        _row(realized=20, q50=5, q80=10, q95=18),
        _row(realized=2, q50=2, q80=5, q95=9, iris=3),
    ]
    s = summarize(rows)
    assert s["n_resolved"] == 4
    bucket = s["buckets"]["15-45"]
    assert bucket["n"] == 4
    assert bucket["coverage"]["q50"] == 0.5  # 0<=1, 5>4, 20>5, 2<=2
    assert bucket["coverage"]["q80"] == 0.75
    # db bias: vendo forecasts all 0 -> mean underestimate = mean(realized)
    assert s["db_bias"]["vendo"]["mean_underestimate_min"] == 6.75
    assert s["db_bias"]["iris"]["n"] == 1
    assert s["cancellation"]["observed_rate"] == 0.0


def test_summarize_cancelled_excluded_from_delay_stats():
    rows = [
        _row(realized=0, q50=1, q80=2, q95=3),
        _row(realized=None, q50=1, q80=2, q95=3, cancelled=1),
    ]
    s = summarize(rows)
    assert s["n_cancelled"] == 1
    assert s["buckets"]["15-45"]["n"] == 1
    assert s["cancellation"]["observed_rate"] == 0.5
