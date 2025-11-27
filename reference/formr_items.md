# Download items from formr

After connecting to formr using
[`formr_connect()`](http://rubenarslan.github.io/formr/reference/formr_connect.md)
you can download items using this command. One of survey_name or path
has to be specified, if both are specified, survey_name is preferred.

## Usage

``` r
formr_items(survey_name = NULL, host = formr_last_host(), path = NULL)
```

## Arguments

- survey_name:

  case-sensitive name of a survey your account owns

- host:

  defaults to
  [`formr_last_host()`](http://rubenarslan.github.io/formr/reference/formr_last_host.md),
  which defaults to https://formr.org

- path:

  path to local JSON copy of the item table

## Examples

``` r
if (FALSE) { # \dontrun{
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
formr_items(survey_name = 'training_diary' )
} # }
formr_items(path = 
  system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))[1:2]
#> $gods
#> $gods$type
#> [1] "mc_button"
#> 
#> $gods$choice_list
#> [1] "gods"
#> 
#> $gods$type_options
#> [1] ""
#> 
#> $gods$name
#> [1] "gods"
#> 
#> $gods$label
#> [1] "Who do you believe in the most?"
#> 
#> $gods$label_parsed
#> [1] "Who do you believe in the most?"
#> 
#> $gods$optional
#> [1] 0
#> 
#> $gods$class
#> NULL
#> 
#> $gods$showif
#> NULL
#> 
#> $gods$value
#> NULL
#> 
#> $gods$block_order
#> NULL
#> 
#> $gods$item_order
#> [1] 1
#> 
#> $gods$choices
#> $gods$choices$glob
#> [1] "Glob"
#> 
#> $gods$choices$cthulhu
#> [1] "Cthulhu"
#> 
#> $gods$choices$spaghetti_monster
#> [1] "Spaghetti Monster"
#> 
#> 
#> 
#> $religiousness_1
#> $religiousness_1$type
#> [1] "mc_button"
#> 
#> $religiousness_1$choice_list
#> [1] "agreement"
#> 
#> $religiousness_1$type_options
#> [1] ""
#> 
#> $religiousness_1$name
#> [1] "religiousness_1"
#> 
#> $religiousness_1$label
#> [1] "He touched me with his noodly appendages."
#> 
#> $religiousness_1$label_parsed
#> [1] "He touched me with his noodly appendages."
#> 
#> $religiousness_1$optional
#> [1] 0
#> 
#> $religiousness_1$class
#> NULL
#> 
#> $religiousness_1$showif
#> NULL
#> 
#> $religiousness_1$value
#> NULL
#> 
#> $religiousness_1$block_order
#> NULL
#> 
#> $religiousness_1$item_order
#> [1] 2
#> 
#> $religiousness_1$choices
#> $religiousness_1$choices$`1`
#> [1] "strongly disagree"
#> 
#> $religiousness_1$choices$`2`
#> [1] "somewhat disagree"
#> 
#> $religiousness_1$choices$`3`
#> [1] "somewhat agree"
#> 
#> $religiousness_1$choices$`4`
#> [1] "strongly agree"
#> 
#> 
#> 
```
