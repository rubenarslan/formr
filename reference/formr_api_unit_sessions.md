# List Per-Unit Sessions in a Run

Returns one row per (participant × unit × iteration) for the run — the
history view that complements
[`formr_api_sessions()`](https://rubenarslan.github.io/formr/reference/formr_api_sessions.md)
(which gives one row per participant with their *current* unit only).

## Usage

``` r
formr_api_unit_sessions(
  run_name,
  session_codes = NULL,
  testing = NULL,
  since = NULL,
  limit = 1000,
  offset = 0
)
```

## Arguments

- run_name:

  Name of the run.

- session_codes:

  Optional character vector — restrict to one or more participants'
  histories.

- testing:

  Filter: TRUE for test sessions only, FALSE for real participants only,
  NULL for both.

- since:

  Optional ISO 8601 datetime string. Returns only unit sessions whose
  `created` is at-or-after this — handy for incremental polling.

- limit:

  Pagination limit (default 1000, max 10000).

- offset:

  Pagination offset (default 0).

## Value

A tidy tibble with columns: `unit_session_id`, `session`, `testing`,
`unit_id`, `unit_type`, `unit_description`, `position`, `iteration`,
`created`, `expires`, `ended`, `expired`, `result`, `state`.

## Details

Use this for trajectory plots (Sankey, alluvial), drop-off analytics,
and debugging stuck participants. The rows arrive ordered by
`(session, created, unit_session_id)`, so
`dplyr::group_by(session) |> dplyr::mutate(next_unit = dplyr::lead(unit_description))`
gives the edges of a trajectory plot directly.

Special units (OverviewScriptPage, ServiceMessagePage, ReminderEmail)
surface with `position = NA` because they live outside the ordered run
flow.
