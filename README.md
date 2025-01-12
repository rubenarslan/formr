# formr
 <!-- badges: start -->
 [![CRAN status](https://www.r-pkg.org/badges/version/formr)](https://CRAN.R-project.org/package=formr)
  [![Codecov test coverage](https://codecov.io/gh/rubenarslan/formr/graph/badge.svg)](https://app.codecov.io/gh/rubenarslan/formr)
  <!-- badges: end -->

## The accompanying R package for the study framework [formr](https://github.com/rubenarslan/formr.org) 

The formr R package provides a few convenience functions that may be useful to the users of formr ([formr.org](https://formr.org)), an online survey framework which heavily relies on R via [openCPU](https://github.com/jeroenooms/opencpu). Some functions may be useful to others too.

Some of the functions conveniently generate individual feedback graphics, some are just shorthands to make certain common operations in formr more palatable to R novices. A large number of functions help connect to formr, extract data, correctly type data (dates, numbers, text etc.), automatically aggregate items of a scale and so on.

The R package can be used inside formr.org, it is always loaded by default.

## documentation
https://rubenarslan.github.io/formr

## install

If you want to install the R package locally (e.g. to connect to formr and fetch the data in a nice format for you), run:

    install.packages("remotes")
    remotes::install_github("rubenarslan/formr")
