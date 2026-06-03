# Transform formr_item_list into a data.frame for ease of use

This function just turns a formr_item_list into a data.frame. The
reason, these lists don't come as data.frames as default is because the
'choices' are a list themselves. When transforming, the choice column
contains a collapsed choice list, which may be less useful for some
purposes.

## Usage

``` r
# S3 method for class 'formr_item_list'
as.data.frame(x, row.names, ...)
```

## Arguments

- x:

  a formr_item_list

- row.names:

  not used

- ...:

  not used

## Value

A data.frame of item metadata, one row per item, with `choices`
collapsed to a comma-separated string.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a live formr server and an authenticated session.
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
as.data.frame(formr_items(survey_name = 'training_diary' ))
} # }
items = formr_items(path = 
system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
items_df = as.data.frame(items)
items_df[1,]
#>        type choice_list type_options name                           label
#> 1 mc_button        gods              gods Who do you believe in the most?
#>                      label_parsed optional class showif value block_order
#> 1 Who do you believe in the most?        0  <NA>   <NA>  <NA>        <NA>
#>   item_order                                                       choices
#> 1          1 glob=Glob,cthulhu=Cthulhu,spaghetti_monster=Spaghetti Monster
#>   index
#> 1     1
```
