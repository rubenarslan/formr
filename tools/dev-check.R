# Local pre-submission check.
# Run from the package root with:  Rscript tools/dev-check.R
#
# Reproduces what CI / R-hub will do, without requiring a GitHub push:
#   1. roxygen2::roxygenise()         -- regenerate man/ + NAMESPACE
#   2. urlchecker::url_check()        -- catch dead links before R CMD check
#   3. rcmdcheck::rcmdcheck(...)      -- normal --as-cran run
#   4. strict re-run with _R_CHECK_DEPENDS_ONLY_ = "true" so Suggests-only
#      packages are temporarily unavailable; catches test setup files that
#      eagerly load Suggests.

stopifnot(requireNamespace("rcmdcheck", quietly = TRUE))
stopifnot(requireNamespace("roxygen2", quietly = TRUE))
stopifnot(requireNamespace("urlchecker", quietly = TRUE))

pkg <- "."

message("[1/4] roxygen2::roxygenise()")
roxygen2::roxygenise(pkg)

message("[2/4] urlchecker::url_check()")
urlchecker::url_check(pkg)

message("[3/4] rcmdcheck::rcmdcheck() --as-cran")
res <- rcmdcheck::rcmdcheck(
  pkg,
  args = c("--no-manual", "--as-cran"),
  error_on = "never",
  quiet = FALSE
)
print(res)

message("[4/4] rcmdcheck::rcmdcheck() with _R_CHECK_DEPENDS_ONLY_=true")
res_strict <- rcmdcheck::rcmdcheck(
  pkg,
  args = c("--no-manual", "--as-cran"),
  env  = c(`_R_CHECK_DEPENDS_ONLY_` = "true",
           `_R_CHECK_FORCE_SUGGESTS_` = "false"),
  error_on = "never",
  quiet = FALSE
)
print(res_strict)
