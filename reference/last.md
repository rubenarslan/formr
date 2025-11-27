# Gives the last non-missing element

Just a simple shorthand to get the last, non-missing argument per
default. Can give more than one element and can include missing
elements. The inverse of
[`first()`](http://rubenarslan.github.io/formr/reference/first.md).

## Usage

``` r
last(x, n = 1, na.rm = TRUE)
```

## Arguments

- x:

  vector of which you want the last element

- n:

  number of elements to take from the end

- na.rm:

  whether to remove missings first, defaults to TRUE

## Examples

``` r
last( c(1:10,NA) )
#> [1] 10
last( c(1:10,NA), 2, TRUE )
#> [1]  9 10
```
