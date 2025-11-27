# knit prefixed

Knit using knitr, but prefix file name to figure and cache folder (to
knit in parallel on e.g. a cluster)

## Usage

``` r
knit_prefixed(input, ...)
```

## Arguments

- input:

  input document

- ...:

  all arguments passed to
  [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html)
