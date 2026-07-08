# render inline text for formr

Render text

## Usage

``` r
formr_inline_render(text, self_contained = TRUE, dir = NULL, ...)
```

## Arguments

- text:

  that will be written to a tmp file and used as the input argument

- self_contained:

  passed to
  [markdown_custom_options](https://rubenarslan.github.io/formr/reference/markdown_custom_options.md)

- dir:

  directory in which the intermediate `knit.Rmd` and the rendered
  `knit.html` are written. Defaults to the working directory inside an
  OpenCPU/formr session and to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) in ordinary R
  sessions; see
  [`formr_render()`](https://rubenarslan.github.io/formr/reference/formr_render.md)
  for details.

- ...:

  all other arguments passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

## Value

A length-1 character string of rendered inline HTML.
