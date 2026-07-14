from trainpunct.quantiles import (
    OptionInput,
    Pmf,
    badge,
    compose_options,
    mixture,
    pmf_from_prediction,
)


def test_pmf_from_prediction_anchoring():
    row = [0.0] * 34
    row[3] = 1.0  # deviation 0
    pmf = pmf_from_prediction(row, offset=3, delay_prognosed=7)
    assert pmf.start == 4  # 7 - 3
    assert pmf.quantile(0.5) == (7, False)


def test_quantile_hand_computed():
    # delays 0..3 with probs .4 .3 .2 .1
    pmf = Pmf(0, [0.4, 0.3, 0.2, 0.1])
    assert pmf.quantile(0.5) == (1, False)
    assert pmf.quantile(0.8) == (2, False)
    assert pmf.quantile(0.95) == (3, True)  # lands in last bin -> open tail
    assert pmf.p_delay_greater(1) == 0.30000000000000004 or abs(pmf.p_delay_greater(1) - 0.3) < 1e-9


def test_mixture_weights():
    a = Pmf(0, [1.0])
    b = Pmf(10, [1.0])
    m = mixture([(0.7, a), (0.3, b)])
    assert m.quantile(0.5) == (0, False)
    assert m.quantile(0.8) == (10, True)
    assert abs(sum(m.probs) - 1.0) < 1e-9


def test_compose_failure_branch_uses_next_option():
    # option A: on time if it works, but 50% failure; option B arrives 30 min later, certain
    a = OptionInput(planned_arrival_min=0, conditional=Pmf(0, [1.0]), p_miss=0.5, p_cancel=0.0)
    b = OptionInput(planned_arrival_min=30, conditional=Pmf(0, [1.0]), p_miss=0.0, p_cancel=0.0)
    results = compose_options([a, b], fallback_headway_min=60)
    res_a = results[0]
    assert res_a.p_fail == 0.5
    assert res_a.conditional_q[50] == 0
    # effective: 50% delay 0, 50% arrive with B => +30 gap + B's delay 0
    assert res_a.effective_q[50] == 0
    assert res_a.effective_q[80] == 30
    # option B fallback: own pmf + headway
    assert results[1].effective_q[50] == 0


def test_compose_cancelled_now_forces_fallback():
    a = OptionInput(planned_arrival_min=0, conditional=Pmf(0, [1.0]), p_miss=0.0,
                    p_cancel=0.0, cancelled_now=True)
    b = OptionInput(planned_arrival_min=45, conditional=Pmf(2, [1.0]), p_miss=0.0, p_cancel=0.0)
    results = compose_options([a, b])
    assert results[0].p_fail == 1.0
    # arrival = 45 min gap + B's 2 min delay
    assert results[0].effective_q[50] == 47


def test_compose_p_fail_combines_miss_and_cancel():
    a = OptionInput(planned_arrival_min=0, conditional=Pmf(0, [1.0]), p_miss=0.2, p_cancel=0.1)
    results = compose_options([a])
    assert abs(results[0].p_fail - (1 - 0.8 * 0.9)) < 1e-9


def test_badge_rules():
    assert badge(0, 3, 0.05, False) == "green"
    assert badge(2, 8, 0.05, False) == "amber"
    assert badge(12, 15, 0.05, False) == "red"
    assert badge(0, 0, 0.5, False) == "red"
    assert badge(0, 0, 0.0, True) == "red"
