# check whether a character string contains another

Just a simple shorthand so that inexperienced R users don't have to use
somewhat complex functions such as
[`grepl()`](https://rdrr.io/r/base/grep.html) and
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
with non-default arguments (e.g. fixed params).

## Usage

``` r
haystack %contains% needle
```

## Arguments

- haystack:

  string in which you search

- needle:

  string to search for

## Examples

``` r
"1, 2, 3, 4, you" %contains% "you"
#> [1] TRUE
"1, 2, 3, 4, you" %contains% 1 # unlike str_detect casts all needles as characters
#> [1] TRUE
"1, 2, 3, 4, you" %contains% 343
#> [1] FALSE
```
