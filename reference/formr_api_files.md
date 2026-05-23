# List files attached to a run

Returns a data frame of all files uploaded to a specific run, including
their public URLs and timestamps.

## Usage

``` r
formr_api_files(run_name)
```

## Arguments

- run_name:

  Name of the run.

## Value

A data.frame containing: id, name, path, url, created, modified.
