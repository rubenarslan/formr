# Backup uploaded files from formr

After connecting to formr using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can backup uploaded files using this command.

## Usage

``` r
formr_backup_files(
  survey_name,
  overwrite = FALSE,
  save_path = paste0(survey_name, "/user_uploaded_files"),
  host = formr_last_host()
)
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- overwrite:

  should existing files be overwritten? defaults to FALSE

- save_path:

  defaults to the survey name

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_backup_files(survey_name = 'training_diary' )
} # }
```
