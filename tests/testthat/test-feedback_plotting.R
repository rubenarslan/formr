library(testthat)
library(ggplot2)

test_that("qplot_on_normal works correctly", {
  # Test with example from documentation
  normed_value <- scale(x = 20, center = 14, scale = 5)
  plot <- qplot_on_normal(normed_value, xlab = "Extraversion")
  
  # Check plot structure
  expect_s3_class(plot, "ggplot")
  
  # Check key plot elements
  expect_true("GeomVline" %in% sapply(plot$layers, function(l) class(l$geom)[1]))
  expect_true("StatFunction" %in% sapply(plot$layers, function(l) class(l$stat)[1]))
  
  # Check labels
  expect_equal(plot$labels$x, "Extraversion")
  expect_equal(plot$labels$y, "Percentage of other people with this value")
})

test_that("feedback_chunk works correctly", {
  chunks3 <- c("You are rather introverted.",
               "You're approximately as extraverted as most people.",
               "You are rather extraverted.")
  
  # Test with 3 chunks
  expect_equal(feedback_chunk(0.7, chunks3), chunks3[2])  # average
  expect_equal(feedback_chunk(-1.5, chunks3), chunks3[1]) # low
  expect_equal(feedback_chunk(1.5, chunks3), chunks3[3])  # high
  
  # Test with 5 chunks
  chunks5 <- c("Very low", "Low", "Average", "High", "Very high")
  expect_equal(feedback_chunk(-2.5, chunks5), chunks5[1]) # very low
  expect_equal(feedback_chunk(-1.5, chunks5), chunks5[2]) # low
  expect_equal(feedback_chunk(0, chunks5), chunks5[3])    # average
  expect_equal(feedback_chunk(1.5, chunks5), chunks5[4])  # high
  expect_equal(feedback_chunk(2.5, chunks5), chunks5[5])  # very high
  
  # Test error for wrong number of chunks
  expect_error(feedback_chunk(0, c("Low", "High")), 
              "Have to provide either three or five chunks")
})

test_that("qplot_on_bar works correctly", {
  # Test with example from documentation
  normed_data <- data.frame(
    variable = c("Extraversion", "Openness", "Agreeableness", 
                "Neuroticism", "Conscientiousness"),
    value = c(-3, 1, -1, 0.5, 2)
  )
  
  plot <- qplot_on_bar(normed_data, title = "Your personality")
  
  # Check plot structure
  expect_s3_class(plot, "ggplot")
  
  # Check data mapping
  expect_equal(plot$data, normed_data)
  
  # Check labels
  expect_equal(plot$labels$title, "Your personality")
  expect_equal(plot$labels$x, "Trait")
  expect_equal(plot$labels$y, "Your value")
  
  # Test with standard errors
  normed_data$se <- c(0.2, 0.3, 0.2, 0.25, 0.4)
  plot_with_se <- qplot_on_bar(normed_data, title = "Your personality")
  
  # Check that error bars were added
  expect_true("GeomLinerange" %in% sapply(plot_with_se$layers, function(l) class(l$geom)[1]))
})

test_that("qplot_on_polar works correctly", {
  # Test with weekdays example
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                "Friday", "Saturday", "Sunday")
  normed_data <- data.frame(
    variable = factor(weekdays, weekdays),
    value = c(0, 1, 0.2, 0.5, 1.5, 2, 1)
  )
  
  plot <- qplot_on_polar(normed_data, title = "Your alcohol consumption")
  
  # Check plot structure
  expect_s3_class(plot, "ggplot")
  
  # Check data mapping
  expect_equal(plot$data, normed_data)
  
  # Check coordinate system
  expect_s3_class(plot$coordinates, "CoordPolar")
  
  # Test with standard errors
  normed_data$se <- rep(0.2, 7)
  plot_with_se <- qplot_on_polar(normed_data)
  
  # Check that error bars were added
  expect_true("GeomLinerange" %in% sapply(plot_with_se$layers, function(l) class(l$geom)[1]))
}) 