# Get and Process Run Results

This is the main function for scientists. It fetches data from the API,
automatically cleans types (dates/numbers), reverses items, computes
scales, and joins everything into one dataframe.

## Usage

``` r
formr_api_results(
  run_name = .formr$run_name,
  ...,
  compute_scales = TRUE,
  join = TRUE,
  remove_test_sessions = TRUE,
  verbose = TRUE
)
```

## Arguments

- run_name:

  Name of the run. Defaults to `.formr$run_name`, which is set
  automatically when the code runs inside an OpenCPU session on
  formr.org – so portable run code can omit this argument.

- ...:

  Filters passed to API (e.g. `surveys = c("Daily", "Intake")`,
  `session_ids = "..."`).

- compute_scales:

  Logical. Should scales (e.g. `extraversion`) be computed from items
  (e.g. `extra_1`, `extra_2`)?

- join:

  Logical. If TRUE (default), joins all surveys into one wide dataframe.

- remove_test_sessions:

  Logical. Filter out sessions marked as testing?

- verbose:

  Logical. Print progress messages?

## Value

A processed tibble with class `formr_results`.
