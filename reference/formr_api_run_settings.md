# Get or Update Run Settings

Retrieve the settings for one or more runs as a tidy data frame, or
update them by providing a named list of new values.

## Usage

``` r
formr_api_run_settings(run_name, settings = NULL)
```

## Arguments

- run_name:

  Name of the run (or a vector of names).

- settings:

  A list of settings to update (e.g.,
  `list(public = 1, locked = TRUE)`). If NULL, returns the current
  settings.

## Value

- If `settings` is NULL: A data.frame/tibble with details for all
  requested runs.

- If `settings` is provided: Invisibly returns TRUE on success.
