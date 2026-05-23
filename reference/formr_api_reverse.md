# Reverse Items and Update Labels

Reverses numeric items ending in 'R' based on metadata bounds.
Critically, it also updates
[`haven::labelled`](https://haven.tidyverse.org/reference/labelled.html)
attributes so that the text labels point to the new, reversed values.

## Usage

``` r
formr_api_reverse(results, item_list)
```

## Arguments

- results:

  A data frame containing the results.

- item_list:

  A data frame containing item metadata.
