# Download run structure from formr

After connecting to formr using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download the study/run structure using this command.

## Usage

``` r
formr_run_structure(run_name, host = formr_last_host())
```

## Arguments

- run_name:

  case-sensitive name of a run your account owns

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_run_structure(run_name = 'training_diary' )
} # }
```
