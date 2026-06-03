# Delete a Run

Permanently deletes a run and all associated data (sessions, results).

## Usage

``` r
formr_api_delete_run(run_name, prompt = TRUE, verbose = TRUE)
```

## Arguments

- run_name:

  Name of the run to delete.

- prompt:

  Logical. If TRUE (default), asks for interactive confirmation; in a
  non-interactive session it errors instead of proceeding unattended.
  Pass `prompt = FALSE` to delete without confirmation (e.g. in
  scripts).

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly `TRUE` (single run) or a named logical vector (multiple runs)
indicating per-run success; `FALSE` if the user declines the prompt.
