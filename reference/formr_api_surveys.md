# List Surveys

Returns a list of all surveys owned by the user.

## Usage

``` r
formr_api_surveys(name_pattern = NULL)
```

## Arguments

- name_pattern:

  Optional. Filter surveys by name (partial match).

## Value

A tibble of surveys (id, name, created, modified, results_table).
