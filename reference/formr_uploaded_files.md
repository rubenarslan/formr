# Download uploaded files from formr

After connecting to formr using
[`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download uploaded files using this command.

## Usage

``` r
formr_uploaded_files(survey_name, host = formr_last_host())
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- host:

  defaults to
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

## Value

A list (parsed JSON) of uploaded-file metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_uploaded_files(survey_name = 'training_diary' )
} # }
```
