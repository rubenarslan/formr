# List all runs

Returns a data frame of all runs accessible to the user, including
status flags and timestamps.

## Usage

``` r
formr_api_runs()
```

## Value

A data.frame containing run details: id, name, title, public (bool),
cron_active (bool), locked (bool), created (POSIXct), modified
(POSIXct).
