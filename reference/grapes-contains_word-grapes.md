# check whether a character string contains another as a word

Looks for a string appearing on its own. This is needed e.g. when
checking whether the replies to a mmc item, stored as a comma-separated
list from 1 to 12 contain option 1 - you wouldn't want to get a hit for
11 and 12. Only works for search terms containing alphanumeric
characters. Just a simple shorthand so that inexperienced R users don't
have to use somewhat complex functions such as
[`grepl()`](https://rdrr.io/r/base/grep.html) and
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html).

## Usage

``` r
haystack %contains_word% needle
```

## Arguments

- haystack:

  string in which you search

- needle:

  string to search for

## Examples

``` r
"1, 3, 4" %contains_word% "1" # TRUE
#> [1] TRUE
"1, 3, 4" %contains_word% 1 # TRUE unlike str_detect casts all needles as characters
#> [1] TRUE
"12, 14, 17" %contains_word% "1" # FALSE even though 12 contains 1
#> [1] FALSE
```
