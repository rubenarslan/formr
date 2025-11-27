# Get the last specified host

This function returns the default or the last specified host if called
without an argument. It changes the host when called with an argument.

## Usage

``` r
formr_last_host(host = NULL)
```

## Arguments

- host:

  defaults to https://formr.org

## Value

the last specified host

## Examples

``` r
formr_last_host("https://formr.org")
#> [1] "https://formr.org"
formr_last_host()
#> [1] "https://formr.org"
```
