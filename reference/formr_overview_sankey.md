# Render a participant-flow Sankey for an overview script

Pulls the run's per-unit interaction history via
[`formr_api_unit_sessions()`](https://rubenarslan.github.io/formr/reference/formr_api_unit_sessions.md)
and renders a plotly Sankey diagram of how participants are moving
through the ordered units.

## Usage

``` r
formr_overview_sankey(
  run_name = .formr$run_name,
  testing = FALSE,
  orientation = "v",
  min_avg_visits_to_annotate = 1.1
)
```

## Arguments

- run_name:

  Name of the run. Defaults to `.formr$run_name`, which formr.org sets
  when an OverviewScriptPage renders.

- testing:

  If `FALSE` (default), only real participants are included. `TRUE` to
  include only test sessions, `NULL` for both.

- orientation:

  Sankey orientation; `"v"` (default) renders top-to-bottom (readable on
  narrow admin pages), `"h"` renders left-to-right.

- min_avg_visits_to_annotate:

  Threshold above which a node's label gets the "avg N visits" suffix.
  Default 1.1 – slightly above exactly-once so single-pass runs stay
  clean.

## Value

A plotly Sankey object, or `NULL` with a message when there are no rows
to plot. Returning `NULL` lets the caller's knitr chunk gracefully
display the message instead of erroring.

## Details

Sankey diagrams can only draw acyclic graphs, but diary / longitudinal
studies revisit the same units across iterations. To stay readable
without losing information, the helper collapses each position to a
single node (counting only the first time a participant visits it) and
surfaces the average per-participant visit count as an "avg N visits"
suffix on the node label. Single-pass runs stay clutter-free (no suffix
when the average is ~1); diary studies show e.g. "p20: Daily mood (avg
14.2 visits)" so the loop density is visible without drawing it.

Designed to be called from an OverviewScriptPage on formr.org, where the
server injects the per-token `.formr$access_token` / `.formr$host` /
`.formr$run_name` environment and
[`formr_api_authenticate()`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
picks them up automatically. Outside an Overview render, set `run_name`
explicitly and call
[`formr_api_authenticate()`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
first.
