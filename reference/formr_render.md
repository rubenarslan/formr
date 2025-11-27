# render text for formr

Render text

## Usage

``` r
formr_render(text, self_contained = FALSE, ...)
```

## Arguments

- text:

  that will be written to a tmp file and used as the input argument

- self_contained:

  passed to
  [markdown_custom_options](http://rubenarslan.github.io/formr/reference/markdown_custom_options.md)

- ...:

  all other arguments passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
