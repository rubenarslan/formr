# Create a new run

Creates one or more new runs on the server. Prints a confirmation
message with the public link for each.

## Usage

``` r
formr_api_create_run(name, verbose = TRUE)
```

## Arguments

- name:

  A character vector of names for the new runs (must be unique).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly returns a data frame containing the `name` and `link` of the
created runs.
