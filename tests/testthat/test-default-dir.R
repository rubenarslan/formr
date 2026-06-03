# Tests for the session-default output directory (CRAN: never write to the
# user's filespace by default). formr_default_dir() is a closure singleton that
# cannot be reset to NULL within a session, so the "clean session" assertions
# run first, before any test sets a value.

test_that("a fresh session has no default and writing helpers refuse the working dir", {
	skip_if_not(is.null(formr_default_dir()), "a session default is already set")

	# the internal resolver errors with an actionable message...
	expect_error(.formr_default_or_stop("save_path"), "no default directory set")
	# ...and so do the public writing helpers, rather than writing to getwd()
	expect_error(formr_backup_study("any_study"), "no default directory set")
})

test_that("formr_default_dir() get/set round-trips and validates input", {
	old <- formr_default_dir()
	on.exit(if (!is.null(old)) formr_default_dir(old), add = TRUE)

	target <- tempdir()
	# setting returns the new value; reading without an arg returns it unchanged
	expect_identical(formr_default_dir(target), target)
	expect_identical(formr_default_dir(), target)

	# once set, the internal resolver returns it instead of erroring
	expect_identical(.formr_default_or_stop("save_path"), target)

	# only a single character path is accepted; a rejected value leaves the
	# stored default intact
	expect_error(formr_default_dir(c("a", "b")), "single directory path")
	expect_error(formr_default_dir(123), "single directory path")
	expect_identical(formr_default_dir(), target)
})
