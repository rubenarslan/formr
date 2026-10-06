# Indicators of automated or careless responding for one participant

Sums the page records of one participant (see
[`dq_parse()`](https://rubenarslan.github.io/formr/reference/dq_parse.md))
and reports, per indicator, what was observed and whether it crosses the
threshold at which it is worth a look.

## Usage

``` r
dq_flags(
  d,
  probe = NA,
  srv_ua = NA,
  sig_agent = NA,
  ch_platform = NA,
  ch_mobile = NA
)
```

## Arguments

- d:

  a data frame of page records, as returned by
  [`dq_parse()`](https://rubenarslan.github.io/formr/reference/dq_parse.md).

- probe:

  what was stored in the participant's `agent_probe` items, as one
  string (people never see these fields, so any text is flagged).

- srv_ua:

  the User-Agent header stored by a `browser` item. It is compared with
  the User-Agent the page read.

- sig_agent:

  the `Signature-Agent` header stored by a `server HTTP_SIGNATURE_AGENT`
  item. AI agents that identify themselves send it.

- ch_platform, ch_mobile:

  the client hints stored by `server HTTP_SEC_CH_UA_PLATFORM` and
  `server HTTP_SEC_CH_UA_MOBILE` items. They are compared with `srv_ua`:
  a User-Agent rewritten on its way to the server leaves them as they
  were.

## Value

A data frame with one row per indicator and the columns `group`
(`"environment"`: the browser and the request, `"input"`: how clicks and
text were made, `"attention"`: time, leaving the page, copy and paste),
`indicator`, `value` (what was observed, as text) and `flag` (`TRUE`
when the indicator crosses its threshold; purely informative indicators
are never flagged). If `d` has no record, a single unflagged row says
so.

## Details

No single indicator proves anything. Several input indicators together,
or any text in an `agent_probe` field, are strong signs that a program
made the input; the attention indicators (leaving the page, copying and
pasting) are context about how a person answered. Review flagged
participants by hand before excluding anyone.

## See also

[`dq_flags_by_session()`](https://rubenarslan.github.io/formr/reference/dq_flags_by_session.md)
for a whole results table.

## Examples

``` r
records <- c(
  '{"v":2,"ld":1,"t":41000,"keys":0,"txt_ch":80,"fld_nokey":1,"pd":4,"pd_ctr":4}',
  '{"v":2,"ld":1,"t":9000,"keys":0,"txt_ch":0,"pd":2,"pd_ctr":2}'
)
flags <- dq_flags(dq_parse(records), probe = "yes, a language model")
flags[flags$flag, ]
#>          group                                             indicator
#> 13 environment                              agent_probe field filled
#> 25       input      text answers mostly not typed or pasted (fields)
#> 27       input share of text typed via keys (keys / chars in fields)
#> 37       input                       mouse presses / at exact centre
#>                    value flag
#> 13 yes, a language model TRUE
#> 25                     1 TRUE
#> 27                     0 TRUE
#> 37                 6 / 6 TRUE
```
