import json
from pathlib import Path

import pytest

FIXTURES = Path(__file__).parent / "fixtures"


@pytest.fixture
def journeys_payload() -> dict:
    return json.loads((FIXTURES / "journeys.json").read_text())


@pytest.fixture
def iris_plan_xml() -> str:
    return (FIXTURES / "iris_plan.xml").read_text()


@pytest.fixture
def iris_fchg_xml() -> str:
    return (FIXTURES / "iris_fchg.xml").read_text()


def predictor_response(n_rows: int, offset: int = 3, n_classes: int = 34,
                       transfer_rows: dict[int, float] | None = None) -> dict:
    """Point-mass-at-prognosis distributions plus configurable transfer scores."""
    predictions = []
    for _ in range(n_rows):
        row = [0.0] * n_classes
        row[offset] = 1.0
        predictions.append(row)
    scores: list[float | None] = [None] * n_rows
    for idx, score in (transfer_rows or {}).items():
        scores[idx] = score
    return {"predictions": predictions, "transfer_scores": scores, "offset": offset}
