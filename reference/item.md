# get item from survey attribute

Shortcut for attributes(survey\$item_name)\$item. Fails with a warning.

## Usage

``` r
item(survey, item_name)
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
item(processed_results, "BFIK_extra_4")
#> $type
#> [1] "rating_button"
#> 
#> $choice_list
#> [1] "BFIK_extra_4"
#> 
#> $type_options
#> [1] "5"
#> 
#> $name
#> [1] "BFIK_extra_4"
#> 
#> $label
#> [1] "__Ich gehe aus mir heraus, bin gesellig.__"
#> 
#> $label_parsed
#> [1] "<strong>Ich gehe aus mir heraus, bin gesellig.</strong>"
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
#> [1] 20
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
