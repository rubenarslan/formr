# Simulate data based on item table

Once you've retrieved an item table using
[`formr_items()`](https://rubenarslan.github.io/formr/reference/formr_items.md)
you can use this function to sample data from the possible choices. At
the moment random data is only generated for choice-type items and
numeric ones, as these are most likely to enter data analysis. Does not
yet handle dates, times, text, locations, colors

## Usage

``` r
formr_simulate_from_items(item_list, n = 300)
```

## Arguments

- item_list:

  the result of a call to
  [`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)

- n:

  defaults to 300

## Examples

``` r
if (FALSE) { # \dontrun{
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
sim = formr_simulate_from_items(item_list = formr_items('training_diary'), n = 100)
summary(lm(pushups ~ pullups, data = sim))
} # }
items = formr_items(path = 
system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
fakedata = formr_simulate_from_items(items, n = 20)
fakedata[1:2,]
#>   id             created               ended            modified
#> 1  1 2026-01-29 12:24:35 2026-01-29 12:26:46 2026-01-29 12:26:46
#> 2  2 2026-02-06 20:28:26 2026-02-06 20:31:05 2026-02-06 20:31:05
#>                gods religiousness_1 religiousness_2R religiousness_3
#> 1 spaghetti_monster               4                1               3
#> 2 spaghetti_monster               1                3               3
#>   religiousness_4 prefer_1 prefer_2
#> 1               1        3        5
#> 2               2        2        4
```
