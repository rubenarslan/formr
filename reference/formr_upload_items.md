# Upload new item table

To automatically create surveys using formr, you can upload survey item
tables from R. Only file uploads are available. The file name determines
the survey name. Updating existing surveys is not implemented and not
recommended (because of the sanity checks we require to prevent data
deletion).

## Usage

``` r
formr_upload_items(survey_file_path, host = formr_last_host())
```

## Arguments

- survey_file_path:

  the path to an item table in csv/json/xlsx etc.

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
if (FALSE) { # \dontrun{
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
items <- system.file('extdata/gods_example_items.json', package = 'formr', 
mustWork = TRUE)
formr_upload_items(items)
} # }
```
