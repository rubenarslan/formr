# Delete a Run

Permanently deletes a run and all associated data (sessions, results).

## Usage

``` r
formr_api_delete_run(run_name, prompt = TRUE)
```

## Arguments

- run_name:

  Name of the run to delete.

- prompt:

  Logical. If TRUE (default), asks for interactive confirmation.

## Value

Invisibly returns TRUE on success.
