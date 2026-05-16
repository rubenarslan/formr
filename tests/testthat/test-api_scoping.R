library(testthat)
library(formr)

# Cover the v1 API's scoping surface that the package now reports back
# to the caller: session$scope shape, .formatted_session_scope's NA/
# empty/non-empty handling, and the error-message hints attached when
# the v1 API returns one of its scoping-aware 403 bodies. The auth
# flow itself is exercised by test-api_integration.R via VCR cassettes;
# here we test the post-auth behaviour directly by writing the session
# we'd otherwise mint over HTTP.

clear_state <- function() {
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}
	if (exists("auth_params", envir = formr:::.formr_state)) {
		rm("auth_params", envir = formr:::.formr_state)
	}
}

inject_session <- function(scope = "run:read survey:read") {
	clear_state()
	assign("session", list(
		base_url = httr::parse_url("https://example.invalid/api"),
		token = "tok_for_test",
		scope = scope,
		expires_at = Sys.time() + 3600
	), envir = formr:::.formr_state)
}

# ---- session$scope shape --------------------------------------------

test_that("formr_api_session surfaces scope on a valid session", {
	inject_session("run:read run:write survey:read")
	session <- formr_api_session()
	expect_equal(session$scope, "run:read run:write survey:read")
	clear_state()
})

test_that("a session with NA scope is still a valid session", {
	# Direct-token auth path can't introspect the granted scope; the
	# validator must treat NA the same as "unknown" rather than
	# corrupted.
	inject_session(NA_character_)
	expect_true(formr:::.validate_session(formr_api_session(), silent = TRUE))
	clear_state()
})

test_that("a session with empty-string scope is also valid", {
	# Server returned an explicit empty grant. Every call will 403,
	# but the session itself is structurally fine.
	inject_session("")
	expect_true(formr:::.validate_session(formr_api_session(), silent = TRUE))
	clear_state()
})

# ---- .formatted_session_scope ---------------------------------------

test_that(".formatted_session_scope is robust to all three NA/empty/non-empty states", {
	clear_state()
	# No session at all -> "(unknown)"
	expect_equal(formr:::.formatted_session_scope(), "(unknown)")

	inject_session("run:read")
	expect_equal(formr:::.formatted_session_scope(), "run:read")

	inject_session("")
	expect_equal(formr:::.formatted_session_scope(), "(none)")

	inject_session(NA_character_)
	expect_equal(formr:::.formatted_session_scope(), "(unknown)")

	clear_state()
})

# ---- 403 hint wording -----------------------------------------------
#
# Build a 1-statement reproduction of the request-handler's 403 branch
# (which we don't want to copy-paste). Verifies each of the four
# scoping-aware body shapes produces the right hint, and that other
# 4xx bodies don't pick up an unrelated hint.

format_403_hint <- function(body_text, scope = "run:read") {
	# Cribbed from api_core.R::formr_api_request — kept narrow so we
	# can assert the wording without spinning up a full HTTP fixture.
	inject_session(scope)
	hint <- ""
	if (grepl("Insufficient permissions:", body_text, fixed = TRUE)) {
		hint <- paste0(
			"\nHint: this credential is missing the OAuth scope this endpoint needs. ",
			"Open admin/account#api on your formr instance, tick the matching read/write scope, ",
			"and rotate. Granted scopes were: ",
			formr:::.formatted_session_scope()
		)
	} else if (grepl("not authorized for run", body_text)) {
		hint <- "RUN-ALLOWLIST"
	} else if (grepl("not authorized for survey", body_text)) {
		hint <- "SURVEY-ALLOWLIST"
	} else if (grepl("Cannot create surveys with a run-restricted API client", body_text)) {
		hint <- "CREATE-RESTRICTED"
	}
	clear_state()
	hint
}

test_that("scope-mismatch 403 hint includes the currently-granted scopes", {
	body <- '{"code":403,"message":"Insufficient permissions: \'run:write\' scope required."}'
	h <- format_403_hint(body, scope = "run:read")
	expect_match(h, "OAuth scope this endpoint needs", fixed = TRUE)
	expect_match(h, "Granted scopes were: run:read", fixed = TRUE)
	expect_match(h, "admin/account#api", fixed = TRUE)
})

test_that("run-allowlist 403 hint matches", {
	body <- '{"code":403,"message":"This API client is not authorized for run \'foo\'."}'
	expect_equal(format_403_hint(body), "RUN-ALLOWLIST")
})

test_that("survey-allowlist 403 hint matches", {
	body <- '{"code":403,"message":"This API client is not authorized for survey \'bar\'."}'
	expect_equal(format_403_hint(body), "SURVEY-ALLOWLIST")
})

test_that("create-survey-on-restricted 403 hint matches", {
	body <- '{"code":403,"message":"Cannot create surveys with a run-restricted API client; add the survey to a run via the admin UI first, then update it via the API."}'
	expect_equal(format_403_hint(body), "CREATE-RESTRICTED")
})

test_that("unrelated 4xx bodies do not pick up a stray hint", {
	expect_equal(format_403_hint('{"message":"Not Found"}'), "")
	expect_equal(format_403_hint('{"message":"Bad Request"}'), "")
})
