# List Sessions in a Run

Returns a tidy data frame of sessions. Can either list all sessions
(with filtering) or fetch specific sessions by their codes.

## Usage

``` r
formr_api_sessions(
  run_name,
  session_codes = NULL,
  active = NULL,
  testing = NULL,
  limit = 1000,
  offset = 0
)
```

## Arguments

- run_name:

  Name of the run.

- session_codes:

  Optional. A character vector of session codes to fetch specific
  details for. If provided, `active`, `limit`, and `offset` are ignored.

- active:

  Filter: TRUE for ongoing, FALSE for finished, NULL for all.

- testing:

  Filter: TRUE for test sessions, FALSE for real users, NULL for all.

- limit:

  Pagination limit (default 1000).

- offset:

  Pagination offset (default 0).

## Value

A combined tibble of session states and details.
