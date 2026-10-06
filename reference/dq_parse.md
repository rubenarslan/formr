# Read the records of formr `data_quality` items

A survey page that holds a `data_quality` item stores one record for
that page: a JSON object of counts, durations and yes/no values about
the browser and about how the answers were made. `dq_parse()` turns such
records into a data frame with one row per page.

## Usage

``` r
dq_parse(x)
```

## Arguments

- x:

  a character vector of JSON records, typically the `data_quality`
  columns of one participant. Missing and empty values, and values that
  are not JSON, are dropped.

## Value

A data frame with one row per record and one column per field; an empty
data frame if `x` holds no record.

## Details

Records never hold what was typed, mouse paths, clipboard content, an IP
address or a device fingerprint. Their fields are described in formr's
documentation for administrators.

## See also

[`dq_flags()`](https://rubenarslan.github.io/formr/reference/dq_flags.md)
to turn the records of one participant into indicators,
[`dq_flags_by_session()`](https://rubenarslan.github.io/formr/reference/dq_flags_by_session.md)
to do so for a whole results table.

## Examples

``` r
dq_parse(c('{"v":2,"ld":1,"keys":12,"pd":3}', NA, '{"v":2,"ld":1,"keys":40,"pd":5}'))
#>   v ld keys pd
#> 1 2  1   12  3
#> 2 2  1   40  5
```
