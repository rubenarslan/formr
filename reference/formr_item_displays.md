# Download detailed result timings and display counts from formr

After connecting to formr using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download detailed times and display counts for each item using
this command.

## Usage

``` r
formr_item_displays(survey_name, host = formr_last_host())
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
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
formr_item_displays(survey_name = 'training_diary' )
} # }
```
