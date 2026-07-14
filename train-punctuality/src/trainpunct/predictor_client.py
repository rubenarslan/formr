"""HTTP client for the self-hosted Bahn-Vorhersage predictor container."""

from __future__ import annotations

from dataclasses import dataclass

import httpx


@dataclass
class PredictorResult:
    predictions: list[list[float]]  # per payload row: pmf over delay deviation classes
    transfer_scores: list[float | None]  # per payload row; None where not a transfer
    offset: int  # index `offset` == deviation 0 from delay_prognosed


class Predictor:
    def __init__(self, base_url: str, timeout: float = 30.0):
        self.base_url = base_url.rstrip("/")
        self._client = httpx.AsyncClient(timeout=timeout)

    async def aclose(self) -> None:
        await self._client.aclose()

    async def rate_journeys(self, columns: dict) -> PredictorResult:
        r = await self._client.post(f"{self.base_url}/rate-journeys/", json=columns)
        r.raise_for_status()
        data = r.json()
        return PredictorResult(
            predictions=data["predictions"],
            transfer_scores=data.get("transfer_scores") or [None] * len(data["predictions"]),
            offset=data["offset"],
        )

    async def raw_delay_predictions(self, columns: dict) -> PredictorResult:
        r = await self._client.post(f"{self.base_url}/raw-delay-predictions/", json=columns)
        r.raise_for_status()
        data = r.json()
        return PredictorResult(
            predictions=data["predictions"],
            transfer_scores=[None] * len(data["predictions"]),
            offset=data["offset"],
        )

    async def healthy(self) -> bool:
        try:
            r = await self._client.get(f"{self.base_url}/training-stats/")
            return r.status_code == 200
        except httpx.HTTPError:
            return False
