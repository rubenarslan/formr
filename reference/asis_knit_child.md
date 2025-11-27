# knit_child as is

This slightly modifies the
[`knitr::knit_child()`](https://rdrr.io/pkg/knitr/man/knit_child.html)
function to have different defaults.

- the environment defaults to the calling environment.

- the output receives the class knit_asis, so that the output will be
  rendered "as is" by knitr when calling inside a chunk (no need to set
  results='asis' as a chunk option).

- defaults to quiet = TRUE

## Usage

``` r
asis_knit_child(
  input = NULL,
  text = NULL,
  ...,
  quiet = TRUE,
  options = NULL,
  envir = parent.frame()
)
```

## Arguments

- input:

  if you specify a file path here, it will be read in before being
  passed to knitr (to avoid a working directory mess)

- text:

  passed to
  [`knitr::knit_child()`](https://rdrr.io/pkg/knitr/man/knit_child.html)

- ...:

  passed to
  [`knitr::knit_child()`](https://rdrr.io/pkg/knitr/man/knit_child.html)

- quiet:

  passed to
  [`knitr::knit_child()`](https://rdrr.io/pkg/knitr/man/knit_child.html)

- options:

  defaults to NULL.

- envir:

  passed to
  [`knitr::knit_child()`](https://rdrr.io/pkg/knitr/man/knit_child.html)

## Details

Why default to the calling environment? Typically this function defaults
to the global environment. This makes sense if you want to use
knit_children in the same context as the rest of the document. However,
you may also want to use knit_children inside functions to e.g.
summarise a regression using a set of commands (e.g. plot some
diagnostic graphs and a summary for a regression nicely formatted).

Some caveats:

- the function has to return to the top-level. There's no way to
  [`cat()`](https://rdrr.io/r/base/cat.html) this from loops or an
  if-condition without without setting results='asis'. You can however
  concatenate these objects with
  [`paste.knit_asis()`](http://rubenarslan.github.io/formr/reference/paste.knit_asis.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# an example of a wrapper function that calls asis_knit_child with an argument
# ensures distinct paths for cache and figures, so that these calls can be looped in parallel
regression_summary = function(model) {
   child_hash = digest::digest(model)
   options = list(
       fig.path = paste0(knitr::opts_chunk$get("fig.path"), child_hash, "-"), 
       cache.path = paste0(knitr::opts_chunk$get("cache.path"), child_hash, "-"))
   asis_knit_child("_regression_summary.Rmd", options = options)
}
} # }
```
