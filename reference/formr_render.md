# render text for formr

Render text

## Usage

``` r
formr_render(text, self_contained = FALSE, dir = NULL, ...)
```

## Arguments

- text:

  that will be written to a tmp file and used as the input argument

- self_contained:

  passed to
  [markdown_custom_options](https://rubenarslan.github.io/formr/reference/markdown_custom_options.md)

- dir:

  directory in which the intermediate `knit.Rmd` and the rendered
  `knit.html` are written. Defaults to the working directory when the
  code runs inside an OpenCPU/formr session (there, the working
  directory is the ephemeral per-request session directory from which
  rforms.org retrieves the rendered page via `getFiles("knit.html")`)
  and to [`tempdir()`](https://rdrr.io/r/base/tempfile.html) in ordinary
  R sessions, so the package never writes into your working directory
  unless you ask it to. Set `options(formr.in_opencpu = TRUE/FALSE)` to
  force either default.

- ...:

  all other arguments passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

## Value

A length-1 character string: the path to the rendered HTML file.
