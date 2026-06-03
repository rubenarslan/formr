# Backup a study

Backup a study by downloading all surveys, results, item displays, run
shuffle, user overview and user details. This function will save the
data in a folder named after the study.

## Usage

``` r
formr_backup_study(
  study_name,
  save_path = NULL,
  host = formr_last_host(),
  overwrite = FALSE
)
```

## Arguments

- study_name:

  case-sensitive name of a study your account owns

- save_path:

  directory to write the backup into. Defaults to a sub-folder named
  after the study inside
  [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md);
  set that (or pass `save_path`) since formr never writes to the working
  directory by default.

- host:

  defaults to
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

- overwrite:

  should existing files be overwritten?

## Value

Invisibly `NULL`; called for its side effect of downloading a whole
study (run structure, surveys, files and results) into `save_path`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_default_dir(tempdir())
formr_backup_study(study_name = 'training_diary' )
} # }
```
