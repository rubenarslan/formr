# Download random groups

formr has a specific module for randomisation. After connecting using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download the assigned random groups and merge them with your
data.

## Usage

``` r
formr_shuffled(run_name, host = formr_last_host())
```

## Arguments

- run_name:

  case-sensitive name of the run in which you randomised participants

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
formr_shuffled(run_name = 'different_drills' )
} # }
```
