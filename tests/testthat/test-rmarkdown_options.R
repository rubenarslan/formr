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

test_that("paste.knit_asis works correctly", {
  # Test with example from documentation
  result <- paste.knit_asis("# Headline 1", "## Headline 2")
  
  # Check class
  expect_s3_class(result, "knit_asis")
  
  # Check content
  expect_equal(as.character(result), "# Headline 1\n\n\n## Headline 2")
}) 