# Processed, aggregated results

This function chains
[`formr_recognise()`](http://rubenarslan.github.io/formr/reference/formr_recognise.md)
and
[`formr_aggregate()`](http://rubenarslan.github.io/formr/reference/formr_aggregate.md)
in sequence. Useful if you want to post-process raw results before
aggregating etc.

## Usage

``` r
formr_post_process_results(
  item_list = NULL,
  results,
  compute_alphas = FALSE,
  fallback_max = 5,
  plot_likert = FALSE,
  quiet = FALSE,
  item_displays = NULL,
  tag_missings = !is.null(item_displays),
  remove_test_sessions = TRUE
)
```

## Arguments

- item_list:

  an item_list, defaults to NULL

- results:

  survey results

- compute_alphas:

  passed to formr_aggregate, defaults to TRUE

- fallback_max:

  passed to formr_reverse, defaults to 5

- plot_likert:

  passed to formr_aggregate, defaults to TRUE

- quiet:

  passed to formr_aggregate, defaults to FALSE

- item_displays:

  an item display table, necessary to tag missings

- tag_missings:

  should missings that result from an item not being shown be
  distinguished from missings due to skipped questions?

- remove_test_sessions:

  by default, formr removes results resulting from test session (animal
  names and null session codes)

## Examples

``` r
results = jsonlite::fromJSON(txt = 
  system.file('extdata/BFI_post.json', package = 'formr', mustWork = TRUE))
items = formr_items(path = 
  system.file('extdata/BFI_post_items.json', package = 'formr', mustWork = TRUE))
item_displays = jsonlite::fromJSON(
  system.file('extdata/BFI_post_itemdisplay.json', package = 'formr', mustWork = TRUE))
processed_results = formr_post_process_results(items, results, item_displays = item_displays,
compute_alphas = FALSE, plot_likert = FALSE)
#> No test users detected.
```
