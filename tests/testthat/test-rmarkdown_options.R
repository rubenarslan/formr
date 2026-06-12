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
# getFiles("knit.html"), so formr_render() must emit a file literally named
# "knit.html" in the working directory. v1.1.1 moved this into tempdir() with a
# random name and broke production rendering; this guards against a recurrence.
test_that("formr_render writes knit.html to the working directory", {
  skip_if_not_installed("rmarkdown")

  wd <- file.path(tempdir(), "formr_render_wd")
  dir.create(wd, showWarnings = FALSE)
  old <- setwd(wd)
  on.exit({ setwd(old); unlink(c("knit.Rmd", "knit.html", "knit_files"), recursive = TRUE) }, add = TRUE)
  unlink(c("knit.Rmd", "knit.html"))

  out <- formr_render("# Hi\n\nThere are `r 1 + 1` types.")

  expect_identical(basename(out), "knit.html")
  expect_true(file.exists(file.path(wd, "knit.html")))
})

test_that("paste.knit_asis works correctly", {
  # Test with example from documentation
  result <- paste.knit_asis("# Headline 1", "## Headline 2")
  
  # Check class
  expect_s3_class(result, "knit_asis")
  
  # Check content
  expect_equal(as.character(result), "# Headline 1\n\n\n## Headline 2")
}) 