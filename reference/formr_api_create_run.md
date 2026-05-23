# Create a new run

Creates one or more new runs on the server. Prints a confirmation
message with the public link for each.

## Usage

``` r
formr_api_create_run(name)
```

## Arguments

- name:

  A character vector of names for the new runs (must be unique).

## Value

Invisibly returns a data frame containing the `name` and `link` of the
created runs.
