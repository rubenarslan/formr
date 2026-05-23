# Backup a study

Downloads the full run structure, all survey items, attached files, and
results. Saves everything into a structured folder.

## Usage

``` r
formr_api_backup_run(run_name, dir = NULL, prompt = TRUE)
```

## Arguments

- run_name:

  Name of the run/study.

- dir:

  Local folder to save data (defaults to run_name).

- prompt:

  Logical. If TRUE (default), asks for confirmation before overwriting.
