# Delete file(s) from a run

Removes file attachment(s) from the run. Accepts a single filename, a
vector of filenames, or a local directory path (which will delete files
on the server that match the names of the files in the local directory).

## Usage

``` r
formr_api_delete_file(run_name, file_name)
```

## Arguments

- run_name:

  Name of the run.

- file_name:

  The name of the file(s) to delete (e.g. "image.png"), or a local
  directory path.
