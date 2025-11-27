# Reverse items based on item table or a fallback_max

Example: If your data contains Extraversion_1, Extraversion_2R and
Extraversion_3, there will be two new variables in the result:
Extraversion_2 (reversed to align with \_1 and \_2) and Extraversion,
the mean score of the three. If you supply an item table, the maximum
possible answer to the item will be used to reverse it. If you don't,
the maximum actually given answer or the fallback_max argument will be
used to reverse it. It's faster to do this without an item table, but
this can lead to problems, if you mis-specify the fallback max or the
highest possible value does not occur in the data.

## Usage

``` r
formr_reverse(results, item_list = NULL, fallback_max = 5)
```

## Arguments

- results:

  survey results

- item_list:

  an item_list, defaults to NULL

- fallback_max:

  defaults to 5 - if the item_list is set to null, we will use this to
  reverse

## Examples

``` r
if (FALSE) { # \dontrun{
formr_connect(email = 'you@example.net', password = 'zebrafinch' )
icar_items = formr_items(survey_name='ICAR',host = 'http://localhost:8888/formr/')
# get some simulated data and aggregate it
sim_results = formr_simulate_from_items(icar_items)
reversed_items = formr_reverse(item_list = icar_items, results = sim_results)
} # }
results = jsonlite::fromJSON(txt = 
  system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
items = formr_items(path = 
  system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
formr_reverse(results, items)
#> Warning: religiousness_2R is not of type labelled and cannot be reversed
#>    session             created            modified               ended
#> 1        1 2016-02-29 10:47:54 2016-02-29 10:48:06 2016-02-29 10:48:06
#> 2        2 2016-02-29 10:48:08 2016-02-29 10:48:11 2016-02-29 10:48:11
#> 3        3 2016-02-29 10:48:13 2016-02-29 10:48:16 2016-02-29 10:48:16
#> 4        4 2016-02-29 10:48:18 2016-02-29 10:48:24 2016-02-29 10:48:24
#> 5        5 2016-02-29 10:48:26 2016-02-29 10:48:36 2016-02-29 10:48:36
#> 6        6 2016-02-29 10:48:38 2016-02-29 10:48:44 2016-02-29 10:48:44
#> 7        7 2016-02-29 10:48:45 2016-02-29 10:48:54 2016-02-29 10:48:54
#> 8        8 2016-02-29 10:48:57 2016-02-29 10:49:03 2016-02-29 10:49:03
#> 9        8 2016-02-29 10:49:05 2016-02-29 10:49:11 2016-02-29 10:49:11
#> 10       9 2016-02-29 10:49:13 2016-02-29 10:49:18 2016-02-29 10:49:18
#> 11      10 2016-02-29 10:49:20 2016-02-29 10:49:27 2016-02-29 10:49:27
#> 12      11 2016-02-29 10:49:29 2016-02-29 10:49:35 2016-02-29 10:49:35
#> 13      12 2016-02-29 10:49:37 2016-02-29 10:49:44 2016-02-29 10:49:44
#> 14      13 2016-02-29 10:49:45 2016-02-29 10:49:52 2016-02-29 10:49:52
#>                 gods religiousness_1 religiousness_2R religiousness_3
#> 1            cthulhu               4                3               2
#> 2               glob               1                5               1
#> 3               glob               1                5               1
#> 4  spaghetti_monster               3                4               3
#> 5            cthulhu               4                3               2
#> 6               glob               2                3               4
#> 7            cthulhu               1                2               3
#> 8               glob               2                5               2
#> 9            cthulhu               2                4               2
#> 10 spaghetti_monster               3                3               3
#> 11              glob               1                5               1
#> 12           cthulhu               2                1               2
#> 13 spaghetti_monster               3                2               3
#> 14           cthulhu               3                1               3
#>    religiousness_4 prefer_1 prefer_2
#> 1                3        4        4
#> 2                1        1        1
#> 3                1        1        1
#> 4                2        4        4
#> 5                1        5        1
#> 6                3        4        3
#> 7                4        1        2
#> 8                1        3        2
#> 9                2        3        3
#> 10               3        4        4
#> 11               2        1        2
#> 12               1        2        1
#> 13               4        5        4
#> 14               4        4        5
```
