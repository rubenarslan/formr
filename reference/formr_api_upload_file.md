# Upload File(s) to Run

Uploads local file(s) to the run. Accepts a single file path, a vector
of file paths, or a directory path (which will upload all files within
that directory).

## Usage

``` r
formr_api_upload_file(run_name, path)
```

## Arguments

- run_name:

  Name of the run.

- path:

  Local path to the file, a vector of paths, or a directory path.

## Value

Invisibly returns a list of server responses.
