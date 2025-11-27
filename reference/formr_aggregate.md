# Aggregate data based on item table

If you've retrieved an item table using
[`formr_items()`](http://rubenarslan.github.io/formr/reference/formr_items.md)
you can use this function to aggregate your multiple choice items into
mean scores. If you do not have a item table (e.g. your data was not
collected using formr, you don't want another HTTP request in a
time-sensitive process). Example: If your data contains Extraversion_1,
Extraversion_2R and Extraversion_3, there will be two new variables in
the result: Extraversion_2 (reversed to align with \_1 and \_2) and
Extraversion, the mean score of the three.

## Usage

``` r
formr_aggregate(
  survey_name,
  item_list = formr_items(survey_name, host = host),
  results = formr_raw_results(survey_name, host = host),
  host = formr_last_host(),
  compute_alphas = FALSE,
  fallback_max = 5,
  plot_likert = FALSE,
  quiet = FALSE,
  aggregation_function = rowMeans,
  ...
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

- compute_alphas:

  deprecated, functionality migrated to codebook package

- fallback_max:

  defaults to 5 - if the item_list is set to null, we will use this to
  reverse

- plot_likert:

  deprecated, functionality migrated to codebook package

- quiet:

  defaults to FALSE - If set to true, likert plots and reliability
  computations are not echoed.

- aggregation_function:

  defaults to rowMeans with na.rm = FALSE

- ...:

  passed to `psych::alpha()`

## Examples

``` r
results = jsonlite::fromJSON(txt = 
  system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
items = formr_items(path = 
  system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
results = formr_recognise(item_list = items, results = results)
agg = formr_aggregate(item_list = items, results = results, 
  compute_alphas = FALSE, plot_likert = FALSE)
#> Warning: results[[item$name]]: There are values outside the labelled range. Reversion will only work if both the minimum and maximum of the range are part of the responses.
agg[, c('religiousness', 'prefer')]
#>    religiousness prefer
#> 1           3.00    4.0
#> 2           1.00    1.0
#> 3           1.00    1.0
#> 4           2.50    4.0
#> 5           2.50    3.0
#> 6           3.00    3.5
#> 7           3.00    1.5
#> 8           1.50    2.5
#> 9           2.00    3.0
#> 10          3.00    4.0
#> 11          1.25    1.5
#> 12          2.50    1.5
#> 13          3.50    4.5
#> 14          3.75    4.5
```
