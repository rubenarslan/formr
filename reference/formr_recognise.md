# Recognise data types based on item table

Once you've retrieved an item table using
[`formr_items()`](http://rubenarslan.github.io/formr/reference/formr_items.md)
you can use this function to correctly type your variables based on the
item table (e.g. formr free text types will be character, but
select_add_one will be factor, dates are also typed as Date, datetimes
as POSIXct).

## Usage

``` r
formr_recognise(
  survey_name = NULL,
  item_list = formr_items(survey_name, host = host),
  results = formr_raw_results(survey_name, host = host),
  host = formr_last_host()
)
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- item_list:

  an item_list, will be auto-retrieved based on survey_name if omitted

- results:

  survey results, will be auto-retrieved based on survey_name if omitted

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

## Examples

``` r
results = jsonlite::fromJSON(txt = 
system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
class(results$created)
#> [1] "character"
items = formr_items(path = 
system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
results = formr_recognise(item_list = items, results = results)
class(results$created)
#> [1] "POSIXct" "POSIXt" 
```
