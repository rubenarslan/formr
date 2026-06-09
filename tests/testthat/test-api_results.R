library(testthat)
library(formr)

# formr_api_fetch_results defaults run_name to .formr$run_name and errors
# if neither the argument nor the env field carries a usable value. The
# happy-path (env value flows into the API request) is exercised here via
# a stub; the full HTTP round-trip is covered by test-api_integration.R.

clear_formr_run_name <- function() {
	if (exists("run_name", envir = .formr)) {
		rm("run_name", envir = .formr)
	}
}

test_that("formr_api_fetch_results errors when run_name is unavailable", {
	clear_formr_run_name()
	expect_error(formr_api_fetch_results(), "run_name is required")
	expect_error(formr_api_fetch_results(run_name = NULL), "run_name is required")
	expect_error(formr_api_fetch_results(run_name = ""), "run_name is required")
})

test_that("formr_api_fetch_results picks up .formr$run_name when set", {
	skip_if_not_installed("mockery")

	clear_formr_run_name()
	on.exit(clear_formr_run_name(), add = TRUE)
	assign("run_name", "scoped-run", envir = .formr)

	captured <- NULL
	mockery::stub(formr_api_fetch_results, "formr_api_request", function(endpoint, ...) {
		captured <<- endpoint
		list()
	})

	formr_api_fetch_results()
	expect_match(captured, "runs/scoped-run/results", fixed = TRUE)
})

# Regression: a `calculate` item can legitimately store strings (e.g. a CSV
# blob read from a file that merely starts with a number). formr_api_recognise
# used to force every "numeric" item through as.numeric(), silently turning
# such columns into all-NA. See https://github.com/rubenarslan/formr/issues/45
test_that("formr_api_recognise keeps string-valued calculate items intact", {
	item_list <- dplyr::tibble(
		name = "mycalc", type = "calculate",
		label = "External CSV blob", choices = list(NULL)
	)
	df <- dplyr::tibble(
		session = c("a", "b"),
		mycalc = c("6136,63,50,woman,man,friendship", "1234,99,40,man,woman,romance")
	)

	out <- formr_api_recognise(item_list, df)

	expect_type(out$mycalc, "character")
	expect_identical(as.vector(out$mycalc), df$mycalc)
	expect_false(any(is.na(out$mycalc)))
})

test_that("formr_api_recognise keeps calculate items as strings even when numeric-looking", {
	item_list <- dplyr::tibble(
		name = "mycalc", type = "calculate",
		label = "A number-like blob", choices = list(NULL)
	)
	df <- dplyr::tibble(session = c("a", "b", "c"), mycalc = c("3.5", "7", "12"))

	out <- formr_api_recognise(item_list, df)

	# calculate is NEVER coerced to numeric, even when every value looks numeric
	expect_type(out$mycalc, "character")
	expect_identical(as.vector(out$mycalc), c("3.5", "7", "12"))
})

test_that("formr_api_recognise leaves number/range items numeric", {
	item_list <- dplyr::tibble(
		name = c("age", "slider"), type = c("number", "range"),
		label = c("Age", "Slider"), choices = list(NULL, NULL)
	)
	df <- dplyr::tibble(session = c("a", "b"), age = c("25", "40"), slider = c("1", "7"))

	out <- formr_api_recognise(item_list, df)

	expect_type(out$age, "double")
	expect_type(out$slider, "double")
	expect_equal(as.vector(out$age), c(25, 40))
})
