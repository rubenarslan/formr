# switch choice values with labels

formr display labels for multiple choice items, but stores their values.
We assume you prefer to analyse the values (e.g. numeric values for
Likert-type items, or English values for international surveys), but
sometimes you may wish to switch this around.

## Usage

``` r
choice_labels_for_values(survey, item_name)
```

## Arguments

- survey:

  survey with item_list attribute

- item_name:

  item name

## Examples

``` r
example(formr_post_process_results)
#> 
#> frm___> results = jsonlite::fromJSON(txt = 
#> frm___+  system.file('extdata/BFI_post.json', package = 'formr', mustWork = TRUE))
#> 
#> frm___> items = formr_items(path = 
#> frm___+  system.file('extdata/BFI_post_items.json', package = 'formr', mustWork = TRUE))
#> 
#> frm___> item_displays = jsonlite::fromJSON(
#> frm___+  system.file('extdata/BFI_post_itemdisplay.json', package = 'formr', mustWork = TRUE))
#> 
#> frm___> processed_results = formr_post_process_results(items, results, item_displays = item_displays,
#> frm___+ compute_alphas = FALSE, plot_likert = FALSE)
#> No test users detected.
table(processed_results$BFIK_extra_4)
#> 
#>  1  2  3  4  5 
#>  1  3  4 11  9 
table(choice_labels_for_values(processed_results, "BFIK_extra_4"))
#> 
#> 1: Trifft überhaupt nicht zu                            2 
#>                            1                            3 
#>                            3                            4 
#>                            4                           11 
#>   5: Trifft voll und ganz zu 
#>                            9 
```
