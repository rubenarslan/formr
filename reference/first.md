# Gives the first non-missing element

Just a simple shorthand to get the first, non-missing argument per
default. Can give more than one element and can include missing
elements. The inverse of
[`last()`](http://rubenarslan.github.io/formr/reference/last.md).

## Usage

``` r
first(x, n = 1, na.rm = TRUE)
```

## Arguments

- x:

  vector of which you want the first element

- n:

  number of elements to take from the beginning

- na.rm:

  whether to remove missings first, defaults to TRUE

## Examples

``` r
first( c(NA,1:10) )
#> [1] 1
first( c(NA, 1:10), 2, TRUE )
#> [1] 1 2
```
