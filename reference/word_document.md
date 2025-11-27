# word_document from rmarkdown, but has an added option not to break on error

Exactly like
[`rmarkdown::word_document()`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html),
but with one added argument

## Usage

``` r
word_document(..., break_on_error = FALSE)
```

## Arguments

- ...:

  all other arguments passed to
  [`rmarkdown::word_document()`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html)

- break_on_error:

  should an error in the R code execution interrupt the rendering or
  should rendering continue, defaults to FALSE
