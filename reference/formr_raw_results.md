# Download data from formr

After connecting to formr using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download data using this command.

## Usage

``` r
formr_raw_results(survey_name, host = formr_last_host())
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_raw_results(survey_name = 'training_diary' )
} # }
```
