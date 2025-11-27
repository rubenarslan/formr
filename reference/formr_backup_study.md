# Backup a study

Backup a study by downloading all surveys, results, item displays, run
shuffle, user overview and user details. This function will save the
data in a folder named after the study.

## Usage

``` r
formr_backup_study(
  study_name,
  save_path = study_name,
  host = formr_last_host(),
  overwrite = FALSE
)
```

## Arguments

- study_name:

  case-sensitive name of a study your account owns

- save_path:

  path to save the study data, defaults to the study name

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

- overwrite:

  should existing files be overwritten?

## Examples

``` r
if (FALSE) { # \dontrun{
formr_backup_study(study_name = 'training_diary' )
} # }
```
