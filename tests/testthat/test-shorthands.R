library(testthat)

test_that("first function works correctly", {
  # Test basic functionality
  expect_equal(first(c(NA, 1:10)), 1)
  expect_equal(first(c(NA, 1:10), 2, TRUE), c(1, 2))
  
  # Test with all NA
  expect_equal(length(first(c(NA, NA, NA))), 0)
  
  # Test without NA removal - should preserve type
  x <- c(NA, 1:10)
  expect_equal(first(x, 1, FALSE), x[1])
})

test_that("last function works correctly", {
  # Test basic functionality
  expect_equal(last(c(1:10, NA)), 10)
  expect_equal(last(c(1:10, NA), 2, TRUE), c(9, 10))
  
  # Test with all NA
  expect_equal(length(last(c(NA, NA, NA))), 0)
  
  # Test without NA removal - should preserve type
  x <- c(1:10, NA)
  expect_equal(last(x, 1, FALSE), x[length(x)])
})

test_that("current function works correctly", {
  # Test basic functionality
  x <- c(1:10, NA)
  expect_equal(current(x), x[length(x)])
  expect_equal(current(1:10), 10)
  
  # Test with single value
  expect_equal(current(5), 5)
})

test_that("finished function works correctly", {
  # Test with example from documentation
  survey <- data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
  expect_equal(finished(survey), 2)
  
  # Test empty survey
  expect_equal(finished(data.frame()), 0)
  
  # Test all NA
  expect_equal(finished(data.frame(ended = c(NA, NA, NA))), 0)
})

test_that("expired function works correctly", {
  # Test with example from documentation
  survey <- data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
  expect_equal(expired(survey), 1)
  
  # Test empty survey
  expect_equal(expired(data.frame()), 0)
  
  # Test all NA
  expect_equal(expired(data.frame(expired = c(NA, NA, NA))), 0)
})

test_that("string operators work correctly", {
  # Test %contains%
  expect_true("1, 2, 3, 4, you" %contains% "you")
  expect_true("1, 2, 3, 4, you" %contains% 1)
  expect_false("1, 2, 3, 4, you" %contains% 343)
  
  # Test %contains_word%
  expect_true("1, 3, 4" %contains_word% "1")
  expect_true("1, 3, 4" %contains_word% 1)
  expect_false("12, 14, 17" %contains_word% "1")
  
  # Test %begins_with%
  expect_true("1, 3, 4" %begins_with% "1")
  expect_true("1, 3, 4" %begins_with% 1)
  expect_false("1, 3, 4" %begins_with% ".")
  
  # Test %ends_with%
  expect_true("1, 3, 4" %ends_with% "4")
  expect_true("1, 3, 4" %ends_with% 4)
  expect_false("1, 3, 4" %ends_with% ".")
})

test_that("if_na works correctly", {
  # Test with example from documentation
  number_of_sex_partners <- c(1, 3, 5, 10, NA, 29)
  expect_equal(if_na(number_of_sex_partners, 0), c(1, 3, 5, 10, 0, 29))
  
  # Test with multiple NAs
  expect_equal(if_na(c(1, NA, 3, NA), 0), c(1, 0, 3, 0))
  
  # Test with vector replacement - should cycle through replacement values
  expect_equal(if_na(c(1, NA, 3, NA), c(10, 20)), c(1, 10, 3, 20))
}) 