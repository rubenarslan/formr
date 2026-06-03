# Backup surveys

Backup surveys by downloading item lists, results, item displays and
file lists.

## Usage

``` r
formr_backup_surveys(
  survey_names,
  surveys = list(),
  save_path = NULL,
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

  directory to write the surveys into. Defaults to
  [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md);
  set that (or pass `save_path`) since formr never writes to the working
  directory by default.

- overwrite:

  should existing files be overwritten?

- host:

  defaults to
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

## Value

Invisibly `NULL`; called for its side effect of downloading surveys
(items, results, item displays and files) into `save_path`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_backup_surveys(survey_names = 'training_diary', save_path = file.path(tempdir(), 'surveys'))
} # }
```
