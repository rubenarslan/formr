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
