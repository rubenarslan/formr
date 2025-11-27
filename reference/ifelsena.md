# Like [`ifelse()`](https://rdrr.io/r/base/ifelse.html), but allows you to assign a third value to missings.

Deprecated. Please use
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
in the future. Defaults to assigning the "no" value to missing values as
well. Often missings encapsulate some sort of meaning for the variable
you're trying to define.

## Usage

``` r
ifelsena(test, yes, no, missing = no)
```

## Arguments

- test:

  passed to ifelse

- yes:

  passed to ifelse

- no:

  passed to ifelse

- missing:

  defaults to the value for no

## Examples

``` r
if (FALSE) { # \dontrun{
data(beavers)
beaver1$activ[1:10] = NA
beaver1$hyperactive = ifelse(beaver1$activ > 1, 1, 0)
table(beaver1$hyperactive)
beaver1$hyperactive = ifelsena(beaver1$activ > 1, 1, 0)
table(beaver1$hyperactive)
} # }
```
