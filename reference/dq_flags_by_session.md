# Indicators of automated or careless responding for every session of a results table

Applies
[`dq_flags()`](https://rubenarslan.github.io/formr/reference/dq_flags.md)
to each row of a results table: the `data_quality` columns of the row
are read as its page records, the `agent_probe` columns as what was
typed into the hidden fields, and the optional columns with request
headers are passed on when they exist.

## Usage

``` r
dq_flags_by_session(
  results,
  pages = "^dq_p[0-9]+$",
  probes = "^dq_agent_p[0-9]+$",
  srv_ua = "dq_srv_ua",
  sig_agent = "dq_sig_agent",
  ch_platform = "dq_ch_platform",
  ch_mobile = "dq_ch_mobile",
  session = "session",
  flagged_only = TRUE
)
```

## Arguments

- results:

  a data frame of survey results with one row per session, for example
  from
  [`formr_results()`](https://rubenarslan.github.io/formr/reference/formr_results.md).

- pages:

  a regular expression that matches the names of the `data_quality`
  columns.

- probes:

  a regular expression that matches the names of the `agent_probe`
  columns.

- srv_ua, sig_agent, ch_platform, ch_mobile:

  the names of the columns that hold the User-Agent header (a `browser`
  item), the `Signature-Agent` header and the two client hints (`server`
  items). A column that does not exist is ignored; `NULL` leaves it out.

- session:

  the name of the column that identifies a session. Row numbers are used
  when there is no such column.

- flagged_only:

  return only the indicators that are flagged (the default), or all of
  them.

## Value

A data frame with the columns `session`, `group`, `indicator`, `value`
and `flag`, one row per session and indicator.

## Details

The defaults match the column names used in formr's example survey
(`dq_p1`, `dq_p2`, ... for the records, `dq_agent_p1`, ... for the
hidden fields). Use your own names or patterns if you named the items
differently.

## See also

[`dq_rrt()`](https://rubenarslan.github.io/formr/reference/dq_rrt.md)
for the randomised-response question.

## Examples

``` r
results <- data.frame(
  session = c("a", "b"),
  dq_p1 = c(
    '{"v":2,"ld":1,"keys":95,"txt_ch":90,"pd":5,"mv":160}',
    '{"v":2,"ld":1,"keys":0,"txt_ch":90,"fld_nokey":1,"pd":5,"pd_ctr":5}'
  ),
  dq_agent_p1 = c(NA, "yes, a language model")
)
dq_flags_by_session(results)
#>   session       group                                             indicator
#> 1       b environment                              agent_probe field filled
#> 2       b       input      text answers mostly not typed or pasted (fields)
#> 3       b       input share of text typed via keys (keys / chars in fields)
#> 4       b       input                       mouse presses / at exact centre
#>                   value flag
#> 1 yes, a language model TRUE
#> 2                     1 TRUE
#> 3                     0 TRUE
#> 4                 5 / 5 TRUE
# how many indicators are flagged per session
table(dq_flags_by_session(results)$session)
#> 
#> b 
#> 4 
```
