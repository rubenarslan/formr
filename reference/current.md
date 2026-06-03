# Gives the last element, doesn't omit missings

Just a simple shorthand to get the current element (in a formr df, where
the last element is always the one from the current session).

## Usage

``` r
current(x)
```

## Arguments

- x:

  vector of which you want the current element

## Value

A length-1 vector (the last element of `x`), keeping `x`'s type.

## Examples

``` r
current( c(1:10,NA) )
#> [1] NA
current( 1:10 )
#> [1] 10
```
