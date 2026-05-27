# checks whether a new day has broken (date has increased by at least one day)

a simple utility functions to avoid that looped Skip Backwards/Skip
Forwards in formr are true repeatedly.

## Usage

``` r
next_day(date = NULL)
```

## Arguments

- date:

  defaults to .formr\$last_action_date, a hidden variable that is
  automatically set by rforms.org. Will be coerced to POSIXct.

## Examples

``` r
next_day(Sys.time())
#> [1] "2026-05-28 UTC"
```
