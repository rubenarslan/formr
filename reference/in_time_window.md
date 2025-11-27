# checks whether the current time is in a certain time window

supply min,max as POSIXct

## Usage

``` r
in_time_window(min, max)
```

## Arguments

- min:

  POSIXct \< max

- max:

  POSIXct \> min

## Examples

``` r
in_time_window(Sys.time() - 1, Sys.time() + 1)
#> [1] TRUE
```
