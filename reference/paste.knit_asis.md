# paste.knit_asis

Helper function for knit_asis objects, useful when e.g.
[`asis_knit_child()`](http://rubenarslan.github.io/formr/reference/asis_knit_child.md)
was used in a loop.

## Usage

``` r
paste.knit_asis(..., sep = "\n\n\n", collapse = "\n\n\n")
```

## Arguments

- ...:

  passed to [`paste()`](https://rdrr.io/r/base/paste.html)

- sep:

  defaults to two empty lines, passed to
  [`paste()`](https://rdrr.io/r/base/paste.html)

- collapse:

  defaults to two empty lines, passed to
  [`paste()`](https://rdrr.io/r/base/paste.html)

## Details

Works like [`paste()`](https://rdrr.io/r/base/paste.html) with both the
sep and the collapse argument set to two empty lines

## Examples

``` r
paste.knit_asis("# Headline 1", "## Headline 2")
#> # Headline 1
#> 
#> 
#> ## Headline 2
```
