# Delete ALL files attached to a run

CAUTION: This will permanently remove every file attached to the
specified run. It first fetches the list of existing files, then
iterates through them to delete.

## Usage

``` r
formr_api_delete_all_files(run_name, prompt = TRUE)
```

## Arguments

- run_name:

  Name of the run.

- prompt:

  Logical. If TRUE (default), the function asks for interactive
  confirmation before deleting. Set to FALSE for automated scripts (use
  with care).
