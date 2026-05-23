# render inline text for formr

Render text

## Usage

``` r
formr_render_commonmark(text)
```

## Arguments

- text:

  that will be passed to knitr

## Examples

``` r
formr_render_commonmark("There are only `r sample(2:3, 1)` types of people.")
#> [1] "<p>There are only 2 types of people.</p>\n"
```
