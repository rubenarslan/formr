# Per-request environment populated by formr.org

An environment that the formr.org server fills with per-request state
when R code runs inside an OpenCPU session on a formr study (for
example, on a `CalculateUnit` page or an `OverviewScriptPage`). Useful
fields the server may set:

## Usage

``` r
.formr
```

## Format

An environment.

## Details

- `.formr$run_name` – the name of the current run.

- `.formr$host` – the API host (e.g. `https://api.formr.org`).

- `.formr$access_token` – a short-lived OAuth token for the request.

- `.formr$last_action_time` / `.formr$last_action_date` – timestamps
  used by
  [`time_passed()`](https://rubenarslan.github.io/formr/reference/time_passed.md)
  and the other shorthands.

Several user-facing functions (e.g.
[`formr_api_authenticate()`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md),
[`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md),
[`formr_overview_sankey()`](https://rubenarslan.github.io/formr/reference/formr_overview_sankey.md))
default their `host` / `run_name` / `access_token` arguments to these
fields, so the same code runs unchanged locally and on a formr study.
Outside a formr session the environment is empty.
