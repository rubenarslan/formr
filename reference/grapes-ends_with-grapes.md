# check whether a character string ends with a string

Escapes any special RegExp characters in the search term. A way to check
whether the search term (e.g. a variable name) is the ending. Just a
simple shorthand so that inexperienced R users don't have to use
somewhat complex functions such as
[`grepl()`](https://rdrr.io/r/base/grep.html) and
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html).

## Usage

``` r
haystack %ends_with% needle
```

## Arguments

- haystack:

  string in which you search

- needle:

  string to search for

## Examples

``` r
"1, 3, 4" %ends_with% "4" # TRUE
#> [1] TRUE
"1, 3, 4" %ends_with% 4 # unlike str_detect casts all needles as characters
#> [1] TRUE
"1, 3, 4" %ends_with% "." # FALSE
#> [1] FALSE
```
