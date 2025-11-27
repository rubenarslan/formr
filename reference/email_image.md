# generates valid email cids

can be used as an argument to
[knitr::opts_knit](https://rdrr.io/pkg/knitr/man/opts_knit.html). If you
attach the images properly, you can then send knit emails including
plots. See the formr OpenCPU module on Github for a sample
implementation.

## Usage

``` r
email_image(x, ext = ".png")
```

## Arguments

- x:

  image ID

- ext:

  extension, defaults to .png

## Examples

``` r
if (FALSE) { # \dontrun{
library(knitr); library(formr)
opts_knit$set(upload.fun=formr::email_image)
} # }
```
