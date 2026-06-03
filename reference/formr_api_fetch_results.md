# Lower-level API Result Fetcher

Fetches raw results. Advanced users can use this if they want completely
raw data without any type coercion or processing.

## Usage

``` r
formr_api_fetch_results(
  run_name = .formr$run_name,
  surveys = NULL,
  session_ids = NULL,
  item_names = NULL,
  join = FALSE
)
```

## Arguments

- run_name:

  Name of the run. Defaults to `.formr$run_name`, which is set
  automatically when the code runs inside an OpenCPU session on
  rforms.org.

- surveys:

  Optional character vector of survey names to filter by.

- session_ids:

  Optional character vector of session IDs to filter by.

- item_names:

  Optional character vector of item names to filter by.

- join:

  Logical. If TRUE, joins the results into a single data frame.

## Value

A tibble of survey results, or (when `join = FALSE`) a named list of
tibbles, one per survey.
