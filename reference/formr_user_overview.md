# Download random groups

formr collects information about users' progression through the run
After connecting using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download a table showing where they are in the run.

## Usage

``` r
formr_user_overview(run_name, host = formr_last_host())
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
formr_user_overview(run_name = 'different_drills' )
} # }
```
