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

## Value

A length-1 character string holding a `cid:` reference for inline email
images, carrying the source path in its `link` attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: meant to run inside a knitr session as the figure upload hook.
library(knitr); library(formr)
opts_knit$set(upload.fun=formr::email_image)
} # }
```
