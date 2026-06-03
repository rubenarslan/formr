# List Surveys

Returns a list of all surveys owned by the user.

## Usage

``` r
formr_api_surveys(name_pattern = NULL, verbose = TRUE)
```

## Arguments

- name_pattern:

  Optional. Filter surveys by name (partial match).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

A tibble of surveys (id, name, created, modified, results_table).
