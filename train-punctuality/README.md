# trainpunct — realistic punctuality forecasts for Aachen Hbf ↔ Witten Hbf

A small self-hosted stack that gives **quantitative, calibrated delay estimates**
(median / 80 % / 95 % arrival times, missed-connection and cancellation risk) for the
Monday-morning Aachen → Witten commute and the Tuesday-afternoon return — instead of
DB's habitually optimistic point forecasts.

**No scraper, no model training of our own.** Predictions come from the excellent
[Bahn-Vorhersage](https://bahnvorhersage.de) project's self-hosted predictor image
(GPLv3, model retrained upstream and republished weekly), which returns full
probability distributions of delay per stop event. This project adds:

- journey enumeration via [transport.rest](https://v6.db.transport.rest) (vendo/bahn.de world),
- delay forecasts fed to the model from the **DB InfraGO IRIS** feed (station world —
  what the model was trained on; bahn.de and the station system are sometimes out of
  sync, and both are shown when they disagree),
- an explicit **cancellation branch** (DB excludes cancelled trains from delay stats;
  the model's distributions are conditional on the train running): a cancelled or
  missed connection re-plans you onto the next viable option, so the app reports
  *effective* arrival quantiles including that risk,
- a **prediction log + outcome resolver**, so after a few travel weeks the calibration
  tab answers, with data, "are these numbers honest for my corridor?" and "how
  optimistic is DB really?",
- a phone-installable **PWA**.

## Architecture

```
┌────────────┐   /journeys    ┌───────────────────┐   POST /rate-journeys/   ┌──────────────────────────┐
│ transport. │ ─────────────► │  app (FastAPI)    │ ───────────────────────► │ bahnvorhersage-predictor │
│ rest       │                │  features adapter │ ◄─────────────────────── │ (their weekly model)     │
└────────────┘                │  quantiles+cancel │    delay distributions   └──────────────────────────┘
┌────────────┐   fchg/plan    │  prediction log   │
│ IRIS       │ ─────────────► │  PWA static       │ ◄── phone (PWA, polls /api/options)
│ (InfraGO)  │                └───────────────────┘
└────────────┘                ┌───────────────────┐
                              │ resolver          │  polls only the day's actual trains,
                              │ (travel days)     │  writes realized outcomes
                              └───────────────────┘
```

## Deploy (your server)

Prerequisites: Linux with Docker Compose v2, ~4 GB free disk, outbound HTTPS.
No API keys needed anywhere.

```sh
cd train-punctuality
cp config.example.yaml config.yaml   # adjust if needed
cp .env.example .env
mkdir -p data

# Verify the EVA numbers in config.yaml once:
docker compose run --rm app trainpunct locations "Witten Hbf"

docker compose up -d --build
curl -s localhost:8300/api/health
open http://<server>:8300/
```

**Verify the predictor contract once after first pull** (the bin semantics were
confirmed from source at build time; the container documents them live):
open `http://<server-internal>:8000/redoc` from the docker network, or:

```sh
docker compose exec app python -c "
import httpx; print(httpx.get('http://predictor:8000/training-stats/').status_code)"
```

### Weekly model refresh

The upstream image is rebuilt weekly with a freshly trained model. Add to crontab:

```cron
0 4 * * 3  cd /path/to/train-punctuality && docker compose pull predictor && docker compose up -d predictor
```

### HTTPS (needed to install the PWA on your phone)

- **Tailscale (recommended, zero config):** `tailscale serve --bg https / http://localhost:8300`
  → install the PWA from `https://<host>.<tailnet>.ts.net/` anywhere on your tailnet.
- **Caddy:** point a (dyn)DNS name at the server and add a `caddy` service with
  `reverse_proxy app:8300` — automatic Let's Encrypt.

### Cancellation-rate table (recommended, one-off)

Corridor-specific cancellation rates from the open
[piebro/deutsche-bahn-data](https://github.com/piebro/deutsche-bahn-data) dumps
(CC BY 4.0 Deutsche Bahn). On any machine with ~2 GB disk:

```sh
pip install "trainpunct[bootstrap] @ ."   # or use the app container
# download 2+ recent monthly parquet files from
# https://huggingface.co/datasets/piebro/deutsche-bahn-data (folder monthly_data/)
trainpunct build-cancel-table monthly_2026-0*.parquet --out data/cancel_table.json
```

Without the table a flat default rate (`cancel_default_rate`, 3 %) applies.

## Using it

- `GET /api/options?day=mon|tue|auto` — ranked options; per option: scheduled vs
  bahn.de forecast vs IRIS forecast vs model `q50/q80/q95` (conditional) and
  `eff_q50/eff_q80/eff_q95` (including missed-connection + cancellation branches,
  re-planned onto the next option), `p_miss`, `p_cancel`, risk badge.
- `GET /api/health`, `GET /api/history?days=30`, `GET /api/calibration`.
- PWA: day toggle (Mo hin / Di zurück / Auto), 60 s auto-refresh, per-leg details
  with source-desync display ("bahn.de +4 / Tafel +11"), calibration tab.

`tail_open: true` on an option means a quantile landed in the model's open last bin
(the upstream model clips delay deviations at +30 min vs the current prognosis) —
read that number as "at least".

## Development

```sh
python3 -m venv .venv && .venv/bin/pip install -e ".[dev]"
.venv/bin/python -m pytest
.venv/bin/ruff check src tests
```

Tests run fully offline against recorded fixtures (transport.rest JSON, IRIS XML,
synthetic predictor distributions).

## Design notes & provenance

- Payload semantics verified against bahnvorhersage source
  (`predictor_webserver/__init__.py`): per stop event the model returns a pmf over
  the *deviation* from the currently prognosed delay; `offset = 3`, 34 classes,
  support `[prognosis − 3, prognosis + 30]`, last bin open-ended. Transfer scores =
  P(minimal transfer time still available), combined from arrival/departure pmfs.
- `features.py` mirrors their `webserver/journeys.py` mapping (GPLv3 — hence this
  subproject's license), with three deliberate deviations: haversine-chain
  `distance_traveled` and start→end `bearing` (they use an internal rail graph we
  don't have; close approximations), and **ISO weekday 1–7** as the model was
  trained (their own frontend sends 0–6 — an off-by-one we don't reproduce).
- The model is trained only on non-cancelled stops, so cancellations are handled as
  an explicit mixture branch here, with empirical rates.
- The resolver prefers IRIS `ct` as ground truth (same definition as the model's
  training labels), falling back to vendo trip refetch.

### Contingency path (if calibration turns out poor for this corridor)

1. Thin conformal recalibration on our own `prediction_log × outcome` data —
   per-horizon additive quantile shifts; no scraper needed.
2. Full own model: training data would come from Bahn-Vorhersage's ODbL
   [open archive](https://bahnvorhersage.de/open-data/) (IRIS history since 2021,
   including timestamped forecast changes) — still no scraper needed.

## Licenses

- This subproject: **GPLv3** (see `LICENSE`) — derived in part from
  [Bahn-Vorhersage](https://gitlab.com/bahnvorhersage/bahnvorhersage) (GPLv3).
- Prediction model/image: Bahn-Vorhersage (GPLv3).
- Cancellation statistics: [piebro/deutsche-bahn-data](https://github.com/piebro/deutsche-bahn-data),
  data CC BY 4.0 Deutsche Bahn.
- Live data: DB InfraGO IRIS feed and transport.rest — be polite, cache, and keep
  request volumes tiny (this app queries only around your actual trips).
