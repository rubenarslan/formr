library(testthat)

test_that("formr_render_commonmark works correctly", {
  # Test with example from documentation
  set.seed(123)  # For reproducibility with sample()
  result <- formr_render_commonmark("There are only `r sample(2:3, 1)` types of people.")
  
  # Should contain a number (2 or 3)
  expect_match(result, "There are only [23] types of people")
  
  # Test with other markdown elements
  result <- formr_render_commonmark("**Bold** and *italic*")
  expect_match(result, "<strong>Bold</strong>")
  expect_match(result, "<em>italic</em>")
})

# Regression: rforms.org/OpenCPU serves the rendered page via
# getFiles("knit.html") from the session working directory, so inside an
# OpenCPU/formr session (simulated here via the formr.in_opencpu option)
# formr_render() must emit a file literally named "knit.html" in the working
# directory. v1.1.1 moved this into tempdir() with a random name and broke
# production rendering; this guards against a recurrence.
test_that("formr_render writes knit.html to the working directory inside OpenCPU", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("withr")

  wd <- file.path(tempdir(), "formr_render_wd")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  on.exit({ setwd(old); unlink(wd, recursive = TRUE) }, add = TRUE)

  withr::local_options(formr.in_opencpu = TRUE)
  out <- formr_render("# Hi\n\nThere are `r 1 + 1` types.")

  expect_identical(basename(out), "knit.html")
  expect_true(file.exists(file.path(wd, "knit.html")))
})

# CRAN policy: in an ordinary R session the default must NOT touch the
# working directory -- rendering goes through tempdir() instead.
test_that("formr_render stays out of the working directory outside OpenCPU", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("withr")

  wd <- file.path(tempdir(), "formr_render_local_wd")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  on.exit({ setwd(old); unlink(wd, recursive = TRUE) }, add = TRUE)

  withr::local_options(formr.in_opencpu = FALSE)
  out <- formr_render("# Hi\n\nThere are `r 1 + 1` types.")

  expect_identical(basename(out), "knit.html")
  expect_identical(normalizePath(dirname(out)), normalizePath(tempdir()))
  expect_false(file.exists(file.path(wd, "knit.html")))
  expect_false(file.exists(file.path(wd, "knit.Rmd")))
})

test_that("formr_render honours an explicit dir argument", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("withr")

  out_dir <- file.path(tempdir(), "formr_render_explicit_dir")
  dir.create(out_dir, showWarnings = FALSE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  withr::local_options(formr.in_opencpu = FALSE)
  out <- formr_render("Hi", dir = out_dir)

  expect_identical(normalizePath(dirname(out)), normalizePath(out_dir))
  expect_true(file.exists(file.path(out_dir, "knit.html")))
})

test_that("in_opencpu() detects the rforms.org per-request environment", {
  skip_if_not_installed("withr")
  skip_if("opencpu" %in% loadedNamespaces())

  # the option override wins in both directions
  withr::with_options(list(formr.in_opencpu = TRUE),
                      expect_true(formr:::in_opencpu()))
  withr::with_options(list(formr.in_opencpu = FALSE),
                      expect_false(formr:::in_opencpu()))

  # outside a formr session nothing is populated
  expect_false(formr:::in_opencpu())

  # rforms.org fills .formr before user code runs
  .formr$host <- "https://api.rforms.org"
  on.exit(.formr$host <- NULL, add = TRUE)
  expect_true(formr:::in_opencpu())
})

test_that("paste.knit_asis works correctly", {
  # Test with example from documentation
  result <- paste.knit_asis("# Headline 1", "## Headline 2")
  
  # Check class
  expect_s3_class(result, "knit_asis")
  
  # Check content
  expect_equal(as.character(result), "# Headline 1\n\n\n## Headline 2")
}) 