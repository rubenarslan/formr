# Backup a study

Downloads the full run structure, all survey items, attached files, and
results. Saves everything into a structured folder.

## Usage

``` r
formr_api_backup_run(run_name, dir = NULL, prompt = TRUE, verbose = TRUE)
```

## Arguments

- run_name:

  Name of the run/study.

- dir:

  Directory to write the backup into. Defaults to a sub-folder named
  after the run inside
  [`formr_default_dir()`](https://rubenarslan.github.io/formr/reference/formr_default_dir.md);
  set that (or pass `dir`) since formr never writes to the working
  directory by default.

- prompt:

  Logical. If TRUE (default), asks for confirmation before overwriting
  when run interactively; in a non-interactive session it errors instead
  of proceeding unattended. Pass `prompt = FALSE` to overwrite without
  confirmation (e.g. in scripts).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `NULL`; called for its side effect of writing the run
structure (JSON), surveys, files and results (`results.rds`) into `dir`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_api_backup_run("my_run", dir = tempdir())
} # }
```
