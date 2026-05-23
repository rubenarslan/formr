library(testthat)
library(formr)

test_that("formr_api_session returns NULL when no session exists", {
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}

	session <- formr_api_session()
	expect_null(session)
})

test_that("formr_api_session validates session structure", {
	session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "test_token_12345",
		expires_at = Sys.time() + 3600
	)

	expect_true(formr:::.validate_session(session, silent = TRUE))
})

test_that("formr_api_session rejects corrupted sessions", {
	expect_error(
		formr:::.validate_session("not_a_list", silent = FALSE),
		"Session corrupted: not a list"
	)

	expect_error(
		formr:::.validate_session(list(token = NULL), silent = FALSE),
		"Session corrupted: missing token"
	)

	expect_error(
		formr:::.validate_session(list(token = ""), silent = FALSE),
		"Session corrupted: invalid token"
	)

	expect_error(
		formr:::.validate_session(list(token = "abc"), silent = FALSE),
		"Session corrupted: missing base_url"
	)
})

test_that("formr_api_session rejects expired sessions", {
	expired_session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "test_token",
		expires_at = Sys.time() - 100
	)

	expect_error(
		formr:::.validate_session(expired_session, silent = FALSE),
		"Session expired"
	)
})

test_that("formr_api_is_authenticated works correctly", {
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}

	expect_false(formr_api_is_authenticated())

	valid_session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "valid_token_12345",
		expires_at = Sys.time() + 3600
	)
	assign("session", valid_session, envir = formr:::.formr_state)
	expect_true(formr_api_is_authenticated())

	expired_session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "expired_token",
		expires_at = Sys.time() - 100
	)
	assign("session", expired_session, envir = formr:::.formr_state)
	expect_false(formr_api_is_authenticated())

	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}
})

test_that("formr_api_token_expiry returns correct information", {
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}

	result <- formr_api_token_expiry()
	expect_null(result$expires_at)
	expect_true(result$is_expired)

	valid_session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "test_token",
		expires_at = Sys.time() + 1800
	)
	assign("session", valid_session, envir = formr:::.formr_state)

	result <- formr_api_token_expiry()
	expect_false(result$is_expired)
	expect_true(result$seconds_left > 0)
	expect_true(result$seconds_left <= 1800)

	expired_session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "test_token",
		expires_at = Sys.time() - 100
	)
	assign("session", expired_session, envir = formr:::.formr_state)

	result <- formr_api_token_expiry()
	expect_true(result$is_expired)
	expect_true(is.na(result$seconds_left) || result$seconds_left < 0)

	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}
})

test_that("formr_api_logout handles missing session gracefully", {
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}

	expect_message(res <- formr_api_logout(), "No active session found")
	expect_false(res)
})

test_that("formr_api_authenticate captures expiry time from OAuth response", {
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}

	session <- list(
		base_url = httr::parse_url("https://formr.org"),
		token = "test_token_123",
		expires_at = Sys.time() + 3600
	)
	assign("session", session, envir = formr:::.formr_state)

	expiry_info <- formr_api_token_expiry()
	expect_false(expiry_info$is_expired)
	expect_true(expiry_info$seconds_left > 0)
	expect_true(inherits(expiry_info$expires_at, "POSIXct"))

	if (exists("session", envir = .formr_state)) {
		rm("session", envir = .formr_state)
	}
})
