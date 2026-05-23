# Create Session(s)

Creates one or more sessions. If `codes` is NULL, one random session is
created. If `codes` is provided, tries to create sessions with those
specific codes.

## Usage

``` r
formr_api_create_session(run_name, codes = NULL, testing = FALSE)
```

## Arguments

- run_name:

  Name of the run.

- codes:

  Character vector of codes. If NULL, creates one random code.

- testing:

  Logical. Mark these sessions as testing?

## Value

Invisibly returns the API response (including created sessions and any
errors).
