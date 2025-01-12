library(testthat)
library(keyring)
library(httr)
library(jsonlite)

# Mock functions to avoid actual HTTP requests
mock_POST_success <- function(...) {
  structure(
    list(
      status_code = 200,
      content = function(...) "Success!"
    ),
    class = "response"
  )
}

mock_POST_fail <- function(...) {
  structure(
    list(
      status_code = 403,
      content = function(...) "<div class='alert-danger'>Invalid credentials</div>"
    ),
    class = "response"
  )
}

test_that("formr_last_host works correctly", {
  # Test default value
  expect_equal(formr_last_host(), "https://formr.org")
  
  # Test setting new host
  formr_last_host("https://example.com")
  expect_equal(formr_last_host(), "https://example.com")
  
  # Test invalid host format
  expect_error(formr_last_host("example.com"), "Host must start with 'https://' or 'http://'")
  
  # Test trailing slash removal
  formr_last_host("https://example.com/")
  expect_equal(formr_last_host(), "https://example.com")
  
  # Reset to default for other tests
  formr_last_host("https://formr.org")
})

test_that("random_date_in_range generates valid dates", {
  lower <- "2020/01/01"
  upper <- "2020/12/31"
  n <- 100
  
  dates <- random_date_in_range(n, lower, upper)
  
  # Check number of dates
  expect_equal(length(dates), n)
  
  # Check all dates are within range
  expect_true(all(dates >= as.POSIXct(lower) & dates <= as.POSIXct(upper)))
  
  # Check dates are sorted
  expect_true(all(diff(dates) >= 0))
})

test_that("email_image generates correct CID format", {
  # Test basic functionality
  result <- email_image("plot_123")
  expect_match(result, "^cid:[a-zA-Z0-9]+\\.png$")
  
  # Test with different extension
  result <- email_image("plot_123", ext = ".jpg")
  expect_match(result, "^cid:[a-zA-Z0-9]+\\.jpg$")
  
  # Test link attribute
  expect_equal(attr(result, "link"), "plot_123")
})

test_that("items and item functions work correctly", {
  # Create test data
  test_df <- data.frame(x = 1:3, y = letters[1:3])
  attr(test_df$x, "item") <- list(
    name = "x",
    label = "Test variable",
    type = "number"
  )
  
  # Test items function
  item_list <- items(test_df)
  expect_true(inherits(item_list, "formr_item_list"))
  expect_equal(length(item_list), 1)
  expect_equal(item_list$x$name, "x")
  
  # Test item function
  item_info <- item(test_df, "x")
  expect_equal(item_info$name, "x")
  expect_equal(item_info$label, "Test variable")
  
  # Test item function with non-existent item
  expect_warning(result <- item(test_df, "z"), "No item information found for this one")
  expect_null(result)
})

test_that("choice_labels_for_values works correctly", {
  # Create test data with labelled values
  test_df <- data.frame(q1 = c(1, 2, 1))
  attr(test_df$q1, "item") <- list(
    name = "q1",
    choices = list(
      "1" = "Agree",
      "2" = "Disagree"
    )
  )
  
  # Test conversion
  result <- choice_labels_for_values(test_df, "q1")
  expect_equal(result, c("Agree", "Disagree", "Agree"))
})

test_that("formr_label_missings handles missing values correctly", {
  # Create test data
  results <- data.frame(
    session = 1:3,
    created = Sys.time() + 1:3,
    q1 = c(1, NA, 3)
  )
  
  item_displays <- data.frame(
    session = 1:3,
    unit_session_id = 1:3,
    item_name = "q1",
    hidden = c(0, 1, 0),
    shown = c(TRUE, FALSE, TRUE)
  )
  
  # Test missing value labeling
  labeled <- formr_label_missings(results, item_displays)
  
  # Check that original non-NA values are preserved
  expect_equal(as.vector(labeled$q1[!is.na(results$q1)]), 
  						 results$q1[!is.na(results$q1)])
  
  # Check that NA values are properly tagged
  expect_true(haven::is_tagged_na(labeled$q1[2]))
})

test_that("formr_post_process_results works correctly", {
  # Load example data from package
  results <- jsonlite::fromJSON(txt = 
    system.file('extdata/BFI_post.json', package = 'formr', mustWork = TRUE))
  items <- formr_items(path = 
    system.file('extdata/BFI_post_items.json', package = 'formr', mustWork = TRUE))
  item_displays <- jsonlite::fromJSON(
    system.file('extdata/BFI_post_itemdisplay.json', package = 'formr', mustWork = TRUE))
  
  # Process results
  processed_results <- formr_post_process_results(
    items, 
    results, 
    item_displays = item_displays,
    compute_alphas = FALSE, 
    plot_likert = FALSE
  )
  
  # Check that test sessions were removed if present
  if("session" %in% names(results)) {
    expect_true(all(!stringr::str_detect(processed_results$session, "XXX")))
  }
  
  # Check that results were recognized (types converted)
  if("created" %in% names(processed_results)) {
    expect_true(inherits(processed_results$created, "POSIXct"))
  }
  
  # Check that missing values were labeled
  expect_true(any(sapply(processed_results, haven::is.labelled)))
})

test_that("formr_aggregate works with example data", {
  # Load example data
  results <- jsonlite::fromJSON(txt = 
    system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
  items <- formr_items(path = 
    system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
  
  # First recognize the data types
  results <- formr_recognise(item_list = items, results = results)
  
  # Then aggregate
  expect_warning(agg <- formr_aggregate(
    item_list = items, 
    results = results, 
    compute_alphas = FALSE, 
    plot_likert = FALSE
  ))
  
  # Check that specified columns exist and are numeric
  expect_true(all(c('religiousness', 'prefer') %in% names(agg)))
  expect_true(is.numeric(agg$religiousness))
  expect_true(is.numeric(agg$prefer))
  
  # Check that aggregation preserved number of rows
  expect_equal(nrow(agg), nrow(results))
})

test_that("formr_recognise works with example data", {
  # Load example data
  results <- jsonlite::fromJSON(txt = 
    system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
  items <- formr_items(path = 
    system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
  
  # Process results
  recognized <- formr_recognise(item_list = items, results = results)
  
  # Check that created field was converted to POSIXct
  expect_true(inherits(recognized$created, "POSIXct"))
  expect_equal(attr(recognized$created, "label"), "user first opened survey")
  
  # Check that choice-based items were properly labelled
  choice_based_items <- sapply(items, function(x) length(x$choices) > 0)
  if(any(choice_based_items)) {
    item_name <- names(items)[which(choice_based_items)[1]]
    if(item_name %in% names(recognized)) {
      expect_true(haven::is.labelled(recognized[[item_name]]))
    }
  }
})

test_that("formr_items works with example data", {
  # Load items from example file
  items <- formr_items(path = 
    system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
  
  # Check class
  expect_true(inherits(items, "formr_item_list"))
  
  # Convert to data frame
  items_df <- as.data.frame(items)
  
  # Check essential columns exist
  expect_true(all(c("name", "type", "label") %in% names(items_df)))
  
  # Check index was added
  expect_true("index" %in% names(items_df))
  expect_equal(items_df$index, 1:nrow(items_df))
})

# Reset formr_last_host to default after all tests
formr_last_host("https://formr.org") 