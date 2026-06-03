# Upload/Update Survey

Uploads a survey structure.

## Usage

``` r
formr_api_upload_survey(
  file_path = NULL,
  google_sheet_url = NULL,
  verbose = TRUE
)
```

## Arguments

- file_path:

  Path to a local file.

- google_sheet_url:

  Google Sheet URL.

- verbose:

  Logical. If TRUE (default), reports progress via
  [`message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly the server response; called to upload or update a survey from
a local file or Google Sheet.
