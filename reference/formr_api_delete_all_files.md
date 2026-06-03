# Delete ALL files attached to a run

CAUTION: This will permanently remove every file attached to the
specified run. It first fetches the list of existing files, then
iterates through them to delete.

## Usage

``` r
formr_api_delete_all_files(run_name, prompt = TRUE, verbose = TRUE)
```

## Arguments

- run_name:

  Name of the run.

- prompt:

  Logical. If TRUE (default), asks for interactive confirmation before
  deleting; in a non-interactive session it errors instead of proceeding
  unattended. Set to FALSE for automated scripts (use with care).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `TRUE` on success; called to delete all files from the run.
