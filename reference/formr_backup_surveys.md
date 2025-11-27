# Backup surveys

Backup surveys by downloading item lists, results, item displays and
file lists.

## Usage

``` r
formr_backup_surveys(
  survey_names,
  surveys = list(),
  save_path = "./",
  overwrite = FALSE,
  host = formr_last_host()
)
```

## Arguments

- survey_names:

  case-sensitive names of surveys your account owns

- surveys:

  a list of survey data (from a run structure), optional

- save_path:

  path to save the study data, defaults to the study name

- overwrite:

  should existing files be overwritten?

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_backup_surveys(survey_names = 'training_diary', save_path = 'surveys')
} # }
```
