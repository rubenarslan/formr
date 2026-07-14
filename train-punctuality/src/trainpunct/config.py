"""Configuration: config.yaml merged with TRAINPUNCT_* environment overrides."""

from __future__ import annotations

import os
from functools import lru_cache
from pathlib import Path

import yaml
from pydantic import BaseModel


class Station(BaseModel):
    eva: int
    name: str = ""


class Window(BaseModel):
    day: int  # ISO weekday, 1 = Monday
    start: str  # "HH:MM" local
    end: str
    direction: str  # outbound | return


class BadgeRules(BaseModel):
    green_q80_max_delay: int = 5
    green_p_miss_max: float = 0.10
    red_q50_min_delay: int = 10
    red_p_miss_min: float = 0.30


class Settings(BaseModel):
    origin: Station
    destination: Station
    windows: dict[str, Window] = {}

    predictor_url: str = "http://predictor:8000"
    transportrest_url: str = "https://v6.db.transport.rest"
    iris_url: str = "https://iris.noncd.db.de"
    journeys_results: int = 8
    options_cache_seconds: int = 75

    min_transfer_default: int = 5
    min_transfer_overrides: dict[int, int] = {}

    cancel_table_path: str = "/data/cancel_table.json"
    cancel_default_rate: float = 0.03
    fallback_headway_min: int = 60

    db_path: str = "/data/trainpunct.sqlite3"

    badge: BadgeRules = BadgeRules()
    desync_show_threshold_min: int = 2

    resolve_grace_min: int = 90
    resolve_poll_seconds: int = 300


ENV_OVERRIDES = {
    "TRAINPUNCT_PREDICTOR_URL": "predictor_url",
    "TRAINPUNCT_TRANSPORTREST_URL": "transportrest_url",
    "TRAINPUNCT_IRIS_URL": "iris_url",
    "TRAINPUNCT_DB_PATH": "db_path",
    "TRAINPUNCT_CANCEL_TABLE_PATH": "cancel_table_path",
}


def load_settings(path: str | Path | None = None) -> Settings:
    path = Path(path or os.environ.get("TRAINPUNCT_CONFIG", "config.yaml"))
    data = {}
    if path.exists():
        data = yaml.safe_load(path.read_text()) or {}
    for env, key in ENV_OVERRIDES.items():
        if os.environ.get(env):
            data[key] = os.environ[env]
    return Settings(**data)


@lru_cache(maxsize=1)
def settings() -> Settings:
    return load_settings()
