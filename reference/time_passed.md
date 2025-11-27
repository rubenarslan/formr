# checks how much time has passed relative to the user's last action

checks how much time has passed. You can choose the unit. Implemented
via
[`lubridate::dseconds()`](https://lubridate.tidyverse.org/reference/duration.html),
not periods, i.e. a minute has 60 seconds, an hour 60 minutes, a day 24
hours. Months and years are not well-defined durations, but we offer
them anyway for convenience. Returns true or false.

## Usage

``` r
time_passed(
  years = 0,
  months = 0,
  weeks = 0,
  days = 0,
  hours = 0,
  minutes = 0,
  seconds = 0,
  time = NULL
)
```

## Arguments

- years:

  365 days

- months:

  30 days

- weeks:

  7 days

- days:

  24 hours

- hours:

  60 minutes

- minutes:

  60 seconds

- seconds:

  argument to
  [`lubridate::dseconds()`](https://lubridate.tidyverse.org/reference/duration.html)

- time:

  defaults to .formr\$last_action_time, a hidden variable that is
  automatically set by formr.org

## Examples

``` r
time_passed(hours = 7, time = Sys.time())
#> [1] FALSE
```
