# get item list from survey attributes

get item list from survey attributes

## Usage

``` r
items(survey)
```

## Arguments

- survey:

  survey with item_list attribute

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
items(processed_results)[[1]]
#> $type
#> [1] "rating_button"
#> 
#> $choice_list
#> [1] "BFIK_open_2"
#> 
#> $type_options
#> [1] "5"
#> 
#> $name
#> [1] "BFIK_open_2"
#> 
#> $label
#> [1] "__Ich bin tiefsinnig, denke gerne über Sachen nach.__"
#> 
#> $label_parsed
#> [1] "<strong>Ich bin tiefsinnig, denke gerne über Sachen nach.</strong>"
#> 
#> $optional
#> [1] 0
#> 
#> $class
#> NULL
#> 
#> $showif
#> NULL
#> 
#> $value
#> NULL
#> 
#> $block_order
#> NULL
#> 
#> $item_order
#> [1] 4
#> 
#> $choices
#> $choices$`1`
#> [1] "1: Trifft überhaupt nicht zu"
#> 
#> $choices$`2`
#> [1] "2"
#> 
#> $choices$`3`
#> [1] "3"
#> 
#> $choices$`4`
#> [1] "4"
#> 
#> $choices$`5`
#> [1] "5: Trifft voll und ganz zu"
#> 
#> 
```
