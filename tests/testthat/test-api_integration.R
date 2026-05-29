library(testthat)
library(formr)

# Whole file relies on vcr cassettes + mockery; bail out before the
# library() calls when either is missing (e.g. CRAN's
# `_R_CHECK_DEPENDS_ONLY_` strict run). Wrapping in if() means the body
# is simply not parsed if the gate trips.
if (requireNamespace("vcr", quietly = TRUE) &&
    requireNamespace("mockery", quietly = TRUE)) {

library(vcr)
library(mockery)

test_that("formr_api_authenticate works (live/recorded)", {
	
	vcr::use_cassette("formr_api_authenticate", {
		
		# 1. Authenticate using env vars (loaded automatically from .Renviron)
		formr_api_authenticate(
			host = Sys.getenv("FORMR_HOST"), 
			client_id = Sys.getenv("FORMR_CLIENT_ID"),
			client_secret = Sys.getenv("FORMR_CLIENT_SECRET")
		)
		
		# 2. Verification
		# FIX: Corrected function name from formr_api_api_session -> formr_api_session
		session <- formr_api_session()
		
		# Check that we got a valid token
		expect_type(session$token, "character")
		expect_true(nchar(session$token) > 10)
		
		# Check the host matches
		expected_hostname <- httr::parse_url(Sys.getenv("FORMR_HOST"))$hostname
		expect_equal(session$base_url$hostname, expected_hostname)
	})
})

test_that("formr_api_runs returns a valid tibble", {
	vcr::use_cassette("formr_api_runs_list", {
		runs <- formr_api_runs()
		
		# Check basics
		expect_s3_class(runs, "data.frame")
		expect_true("id" %in% names(runs))
		expect_true("public" %in% names(runs))
		
		# Check type conversion
		expect_type(runs$public, "logical")
	})
})

test_that("formr_api_runs creates a run successfully", {
	vcr::use_cassette("formr_api_runs_create", {
		res <- formr_api_create_run("test-run")
		expect_s3_class(res, "data.frame")
		expect_true(res$name == "test-run")
	})
})

test_that("formr_api_run_structure can import (PUT) a valid JSON", {
	vcr::use_cassette("formr_api_run_structure_import", {
		
		minimal_structure <- '{
      "name": "test-run",
      "units": [
        {
          "type": "Pause",
          "position": 10,
          "wait_minutes": 10
        }
      ]
    }'
		
		tmp_json <- tempfile(fileext = ".json")
		writeLines(minimal_structure, tmp_json)
		on.exit(unlink(tmp_json))
		
		success <- formr_api_run_structure(run_name = "test-run", structure_json_path = tmp_json)
		expect_true(success)
		
		imported <- formr_api_run_structure("test-run")
		unit_types <- sapply(imported$units, function(x) x$type)
		expect_true("Pause" %in% unit_types)
	})
})

test_that("formr_api_create_session creates a session and returns code", {
	vcr::use_cassette("formr_api_create_session_basic", {
		res <- formr_api_create_session(run_name = "test-run", testing = TRUE)
		expect_type(res$sessions, "list") 
		expect_length(res$sessions, 1)
		expect_type(res$sessions[[1]], "character")
	}, match_requests_on = c("method", "uri", "body")) 
})

test_that("formr_api_session_action moves user position", {
	vcr::use_cassette("formr_api_session_action_move", {
		session_res <- formr_api_create_session("test-run", testing = TRUE)
		code <- session_res$sessions[[1]]
		
		success <- formr_api_session_action(
			run_name = "test-run", 
			session_codes = code, 
			action = "move_to_position", 
			position = 10
		)
		expect_true(success)
		
		details <- formr_api_sessions("test-run", code)
		expect_equal(as.numeric(details$position), 10)
	}, match_requests_on = c("method", "uri", "body"))
})

test_that("formr_api_results works (Single DF Auto-Detection)", {
	vcr::use_cassette("formr_api_results_fetch_single", {
		results <- formr_api_results(run_name = "test-run", join = TRUE)
		
		expect_s3_class(results, "tbl_df")
		expect_true("session" %in% names(results))
		if ("created" %in% names(results)) {
			expect_s3_class(results$created, "POSIXct")
		}
	})
})

test_that("formr_api_survey_structure parses nested choices correctly", {
	vcr::use_cassette("formr_api_survey_structure_items", {
		items <- formr_api_survey_structure("platzhalter")
		expect_s3_class(items, "tbl_df")
		expect_true(all(c("name", "type", "label") %in% names(items)))
		if ("choices" %in% names(items)) {
			expect_type(items$choices, "list")
		}
	})
})

test_that("formr_api_upload_file works (multipart request)", {
	vcr::use_cassette("formr_api_upload_delete_flow", {
		# Run file ops in a tempdir (never the home/working dir) but keep the
		# original basename so the recorded multipart upload + DELETE URI match.
		withr::with_tempdir({
			tmp_file <- "test_upload.txt"
			writeLines("This is a test file", tmp_file)

			res <- formr_api_upload_file(run_name = "test-run", path = tmp_file)
			files <- formr_api_files("test-run")
			expect_true(tmp_file %in% files$name)

			formr_api_delete_file("test-run", tmp_file)
			files_after <- formr_api_files("test-run")
			expect_false(tmp_file %in% files_after$name)
		})
	}, match_requests_on = c("method", "uri"))
})

test_that("formr_api_survey_structure returns valid item metadata", {
	vcr::use_cassette("formr_api_survey_structure_fetch", {
		surveys <- formr_api_surveys()
		if (nrow(surveys) == 0) skip("No surveys found on this account to test.")
		
		target_survey <- surveys$name[1]
		items <- formr_api_survey_structure(target_survey)
		
		expect_s3_class(items, "tbl_df")
		if ("choices" %in% names(items)) {
			expect_type(items$choices, "list")
		}
	})
})

test_that("get_api_project_state respects .formrignore", {
	withr::with_tempdir({
		dir.create("backup")
		dir.create("surveys")
		writeLines("ignored", "backup/secret.rds")
		writeLines("ignored", ".DS_Store")
		writeLines("tracked", "surveys/my_survey.xlsx")
		
		# Create .formrignore
		writeLines(c("backup/", ".DS_Store"), ".formrignore")
		
		# We need to access the internal function. 
		# Use formr::: if it's not exported, or test via public API behavior.
		state <- formr:::get_api_project_state(".")
		
		files <- names(state)
		expect_true("surveys/my_survey.xlsx" %in% files)
		expect_false("backup/secret.rds" %in% files)
		expect_false(".DS_Store" %in% files)
	})
})

test_that("formr_api_runs deletes a run successfully", {
	vcr::use_cassette("formr_api_runs_delete", {
		res <- formr_api_delete_run("test-run", prompt = FALSE)
		expect_true(res)
	})
})

test_that("formr_api_logout handles invalid and valid sessions correctly", {
	
	# --- Scenario 1: Logout when already logged out ---
	# Ensure we start with a clean slate
	if (exists("session", envir = formr:::.formr_state)) {
		rm("session", envir = formr:::.formr_state)
	}
	
	# Expect a specific message and FALSE return
	expect_message(
		res <- formr_api_logout(), 
		"No active session found"
	)
	expect_false(res)
	
	
	# --- Scenario 2: Full Logout Flow (Auth -> Logout -> Verify) ---
	vcr::use_cassette("formr_api_logout_flow", {
		
		# 1. Setup: Authenticate to get a valid session
		# We use the existing auth function to populate .formr_state$session
		formr_api_authenticate(
			host = Sys.getenv("FORMR_HOST"), 
			client_id = Sys.getenv("FORMR_CLIENT_ID"),
			client_secret = Sys.getenv("FORMR_CLIENT_SECRET")
		)
		
		# Verify we actually have a session to kill
		session_initial <- formr_api_session()
		expect_false(is.null(session_initial))
		expect_type(session_initial$token, "character")
		
		# 2. Action: Call Logout
		# We expect two messages: one for server revocation, one for local clearing.
		# wrapping in capture_messages to verify both.
		msgs <- capture_messages(res <- formr_api_logout())
		
		# 3. Assertions
		
		# Check messages (Flexible matching in case order/wording tweaks vary slightly)
		expect_true(any(grepl("Token revoked on server", msgs)))
		expect_true(any(grepl("Local session cleared", msgs)))
		
		# Check return value
		expect_true(res)
		
		# 4. Verification: Check Local State
		# formr_api_session() should now return NULL
		expect_null(formr_api_session())
		
	}, match_requests_on = c("method", "uri", "body"))
})

test_that("Logic Unit Test: Reversal, Types, and Scales work with hardcoded data", {
	
	# 1. HARDCODE METADATA (What the API structure endpoint would return)
	# We simulate a simple scale: 'extra_1' and 'extra_2R' (Reverse coded)
	fake_metadata <- dplyr::tibble(
		name = c("extra_1", "extra_2R", "open_q"),
		type = c("rating_button", "rating_button", "text"),
		label = c("I am outgoing", "I am shy", "Any comments?"),
		choices = list(
			list("1"="Dis", "2"="Neu", "3"="Agr"), # Choices for extra_1
			list("1"="Dis", "2"="Neu", "3"="Agr"), # Choices for extra_2R
			NULL                                    # No choices for text
		)
	)
	
	# 2. HARDCODE RAW DATA (What the API results endpoint would return)
	# - Session 1: answered 1 and 3 (Reverse of 3 in 1..3 range is 1) -> Mean should be (1+1)/2 = 1
	# - Session 2: answered 3 and 1 (Reverse of 1 in 1..3 range is 3) -> Mean should be (3+3)/2 = 3
	fake_results <- dplyr::tibble(
		session = c("s1", "s2"),
		extra_1 = c(1, 3), # Numeric raw data
		extra_2R = c(3, 1), 
		open_q = c("Hello", "World"),
		created = c("2024-01-01 10:00:00", "2024-01-01 10:05:00")
	)
	
	# 3. RUN THE PIPELINE MANUALLY (Bypassing formr_api_results wrapper)
	
	# Step A: Recognize Types (Applies labels/types)
	step1 <- formr_api_recognise(fake_metadata, fake_results)
	expect_true(inherits(step1$created, "POSIXct")) # Check date conversion
	
	# Step B: Reverse Items
	step2 <- formr_api_reverse(step1, fake_metadata)
	
	# Check Reversal Logic: 
	# extra_2R was 3. Range is 1-3. Reverse: (1+3) - 3 = 1.
	expect_equal(as.numeric(step2$extra_2R[1]), 1) 
	expect_true(attr(step2$extra_2R, "reversed")) # Check flag
	
	# Step C: Aggregate Scales
	# We need to tweak the function call slightly if you have a min_items default
	# (Since we only have 2 items, ensure min_items <= 2)
	step3 <- formr_api_aggregate(step2, fake_metadata, min_items = 2)
	
	# Check Scale Calculation
	# Row 1: extra_1=1, extra_2R (reversed)=1. Mean = 1.
	expect_true("extra" %in% names(step3))
	expect_equal(step3$extra[1], 1)
	
	# Row 2: extra_1=3, extra_2R (reversed)=3. Mean = 3.
	expect_equal(step3$extra[2], 3)
})

} # close: if (requireNamespace vcr & mockery)

