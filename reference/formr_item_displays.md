# Download detailed result timings and display counts from formr

After connecting to formr using
[`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)
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
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

## Value

A data.frame (parsed JSON) of item-display records with timing and
display counts.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
formr_item_displays(survey_name = 'training_diary' )
} # }
```
