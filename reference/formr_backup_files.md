# Backup uploaded files from formr

After connecting to formr using
[`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)
you can backup uploaded files using this command.

## Usage

``` r
formr_backup_files(
  survey_name,
  overwrite = FALSE,
  save_path = NULL,
  host = formr_last_host()
)
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- overwrite:

  should existing files be overwritten? defaults to FALSE

- save_path:

  directory to write the files into. Defaults to a sub-folder named
  after the survey inside
  [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md);
  set that (or pass `save_path`) since formr never writes to the working
  directory by default.

- host:

  defaults to
  [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://rforms.org

## Value

Invisibly the file list with an updated `downloaded` field; called to
download a survey's user-uploaded files into `save_path`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_backup_files(survey_name = 'training_diary', save_path = tempdir() )
} # }
```
