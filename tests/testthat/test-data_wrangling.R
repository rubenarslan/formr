library(testthat)
library(haven)

test_that("reverse_labelled_values works as expected", {
  # Test based on example from documentation
  x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
  reversed <- reverse_labelled_values(x)
  
  # Check values are reversed
  expect_equal(as.numeric(reversed[x == 1]), rep(5, 3))
  expect_equal(as.numeric(reversed[x == 2]), rep(4, 3))
  expect_equal(as.numeric(reversed[x == 3]), rep(3, 3))
  
  # Check labels are preserved correctly
  expect_equal(attr(reversed, "labels"), c(Bad = 5, Good = 1))
})

test_that("reverse_labelled_values handles factors", {
  # Test factor conversion
  x <- factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High"))
  expect_warning(reversed <- reverse_labelled_values(x), 
                "Turning a factor into a labelled numeric vector")
  
  # Check conversion worked correctly
  expect_equal(as.numeric(reversed), c(3, 2, 1))
})

test_that("as_same_type_as converts types correctly", {
  # Test numeric conversion
  expect_equal(as_same_type_as(1.0, "42"), 42)
  
  # Test character conversion
  expect_equal(as_same_type_as("a", 42), "42")
})

test_that("aggregate_and_document_scale works as expected", {
  # Test based on example from documentation
  testdf <- data.frame(
    bfi_neuro_1 = rnorm(20),
    bfi_neuro_2 = rnorm(20),
    bfi_neuro_3R = rnorm(20)
  )
  item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
  
  result <- aggregate_and_document_scale(testdf[, item_names])
  
  # Check attributes
  expect_equal(attr(result, "scale_item_names"), item_names)
  expect_match(attr(result, "label"), "3 bfi_neuro items aggregated by")
  
  # Check aggregation
  expect_equal(as.vector(result), 
  						 as.vector(rowMeans(testdf[, item_names], na.rm = T)))
})

test_that("rescue_attributes preserves attributes correctly", {
  # Create test data frames
  df_with <- data.frame(x = 1:3, y = letters[1:3])
  attr(df_with$x, "label") <- "test label"
  attr(df_with$x, "format.spss") <- "F8.2"
  
  df_without <- data.frame(x = 1:3, y = letters[1:3])
  
  # Test attribute rescue
  result <- rescue_attributes(df_without, df_with)
  
  # Check attributes were copied
  expect_equal(attr(result$x, "label"), "test label")
  expect_equal(attr(result$x, "format.spss"), "F8.2")
  
  # Check original values weren't changed
  expect_equal(as.vector(result$x), 1:3)
  expect_equal(result$y, letters[1:3])
})

test_that("rescue_attributes doesn't overwrite existing attributes", {
  df_with <- data.frame(x = 1:3)
  attr(df_with$x, "label") <- "old label"
  attr(df_with$x, "format.spss") <- "F8.2"
  
  df_without <- data.frame(x = 1:3)
  attr(df_without$x, "label") <- "new label"
  
  result <- rescue_attributes(df_without, df_with)
  
  # Check existing attribute wasn't overwritten
  expect_equal(attr(result$x, "label"), "new label")
  
  # Check new attribute was added
  expect_equal(attr(result$x, "format.spss"), "F8.2")
}) 
