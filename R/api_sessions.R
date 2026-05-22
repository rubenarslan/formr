#' List Sessions in a Run
#' 
#' Returns a tidy data frame of sessions. Can either list all sessions (with filtering)
#' or fetch specific sessions by their codes.
#' 
#' @param run_name Name of the run.
#' @param session_codes Optional. A character vector of session codes to fetch specific details for. 
#'   If provided, `active`, `limit`, and `offset` are ignored.
#' @param active Filter: TRUE for ongoing, FALSE for finished, NULL for all.
#' @param testing Filter: TRUE for test sessions, FALSE for real users, NULL for all.
#' @param limit Pagination limit (default 1000).
#' @param offset Pagination offset (default 0).
#' @return A combined tibble of session states and details.
#' @export
formr_api_sessions <- function(run_name, session_codes = NULL, active = NULL, testing = NULL, limit = 1000, offset = 0) {
	
	# --- MODE A: Fetch specific codes ---
	if (!is.null(session_codes)) {
		
		fetch_one <- function(code) {
			tryCatch({
				formr_api_request(
					endpoint = paste0("runs/", run_name, "/sessions/", code), 
					method = "GET"
				)
			}, error = function(e) {
				warning(paste0("[WARNING] Failed to fetch '", code, "': ", e$message), call. = FALSE)
				return(NULL)
			})
		}
		
		# Fetch Loop
		results_list <- lapply(session_codes, fetch_one)
		
		# Filter NULLs (failed requests)
		results_list <- Filter(Negate(is.null), results_list)
		
		return(.process_api_session_data(results_list))
	}
	
	# --- MODE B: List/Search sessions ---
	query <- list(limit = limit, offset = offset)
	if (!is.null(active)) query$active <- if(active) 1 else 0
	if (!is.null(testing)) query$testing <- if(testing) 1 else 0
	
	# Fetch
	res <- formr_api_request(paste0("runs/", run_name, "/sessions"), query = query)
	
	if (length(res) == 0) {
		message(sprintf("[INFO] No sessions found for run '%s'.", run_name))
		return(dplyr::tibble())
	}
	
	return(.process_api_session_data(res))
}

#' Create Session(s)
#' 
#' Creates one or more sessions. If `codes` is NULL, one random session is created.
#' If `codes` is provided, tries to create sessions with those specific codes.
#' 
#' @param run_name Name of the run.
#' @param codes Character vector of codes. If NULL, creates one random code.
#' @param testing Logical. Mark these sessions as testing?
#' @return Invisibly returns the API response (including created sessions and any errors).
#' @export
formr_api_create_session <- function(run_name, codes = NULL, testing = FALSE) {
	body <- list(testing = if(testing) 1 else 0)
	
	# API expects 'code' to be an array if multiple, or a single value
	# Source: ApiHelperV1.php createSession
	if (!is.null(codes)) {
		body$code <- codes
	}
	
	res <- formr_api_request(
		endpoint = paste0("runs/", run_name, "/sessions"), 
		method = "POST", 
		body = body
	)
	
	# Provide user feedback based on the complex response structure (201, 207, 400)
	# Source: ApiHelperV1.php createSession
	count_created <- if(!is.null(res$count_created)) res$count_created else 0
	count_failed <- if(!is.null(res$count_failed)) res$count_failed else 0
	
	if (count_created > 0) {
		message(sprintf("[SUCCESS] Successfully created %d session(s).", count_created))
		# Print the first few created codes
		if(length(res$sessions) > 0) {
			shown <- head(res$sessions, 5)
			cat(paste0("   Codes: ", paste(shown, collapse = ", ")))
			if(length(res$sessions) > 5) cat(" ...")
			cat("\n")
		}
	}
	
	if (count_failed > 0) {
		warning(sprintf("[WARNING] Failed to create %d session(s).", count_failed))
		# Print the errors
		if(!is.null(res$errors)) {
			err_df <- dplyr::bind_rows(res$errors)
			print(err_df)
		}
	}
	
	invisible(res)
}

#' Perform Action on Session(s)
#' 
#' Controls the flow of one or more sessions.
#' 
#' @param run_name Name of the run.
#' @param session_codes A single code or vector of session codes.
#' @param action One of: "end_external", "toggle_testing", "move_to_position", "execute", "advance".
#' @param position Required only if action is "move_to_position".
#' @return A logical vector indicating success for each session.
#' @export
formr_api_session_action <- function(run_name, session_codes, action, position = NULL) {
	
	# 1. Validation
	valid_actions <- c("end_external", "toggle_testing", "move_to_position", "execute", "advance")
	if (!action %in% valid_actions) {
		stop("Invalid action. Must be one of: ", paste(valid_actions, collapse = ", "))
	}
	
	if (action == "move_to_position" && is.null(position)) {
		stop("Argument 'position' is required for action 'move_to_position'.")
	}
	
	# 2. Define single-action helper
	perform_one <- function(code) {
		tryCatch({
			body <- list(action = action)
			if (!is.null(position)) body$position <- position
			
			# Source: ApiHelperV1.php performSessionAction
			formr_api_request(
				endpoint = paste0("runs/", run_name, "/sessions/", code, "/actions"), 
				method = "POST", 
				body = body
			)
			return(TRUE)
			
		}, error = function(e) {
			warning(sprintf("[WARNING] Failed to perform action on '%s': %s", code, e$message))
			return(FALSE)
		})
	}
	
	# 3. Iterate over all codes
	# We use vapply to get a clean named logical vector back
	results <- vapply(session_codes, perform_one, FUN.VALUE = logical(1))
	
	# 4. Feedback
	success_count <- sum(results)
	total_count <- length(session_codes)
	
	if (success_count == total_count) {
		message(sprintf("[SUCCESS] Action '%s' successfully performed on all %d session(s).", action, total_count))
	} else if (success_count > 0) {
		message(sprintf("[INFO] Action '%s' performed on %d/%d session(s). (See warnings for failures)", action, success_count, total_count))
	} else {
		warning(sprintf("[FAILED] Action '%s' failed for all %d session(s).", action, total_count))
	}
	
	invisible(results)
}

#' Process Raw Session List (Internal Helper)
#' 
#' Converts a list of raw session objects (from JSON) into a tidy tibble.
#' Handles type conversion and flattening of nested unit data.
#' 
#' @param session_list A list of lists (raw API response)
#' @return A tidy tibble
#' @noRd
.process_api_session_data <- function(session_list) {
	if (length(session_list) == 0) return(dplyr::tibble())
	
	session_list <- lapply(session_list, function(x) {
		if (!is.null(x$current_unit) && is.list(x$current_unit)) {
			x$current_unit <- list(x$current_unit)
		}
		return(x)
	})
	
	# 1. Bind to initial data frame
	df <- dplyr::bind_rows(session_list)
	
	# 2. Standard Type Conversion
	# We check for column existence to be safe
	if ("created" %in% names(df)) df$created <- as.POSIXct(df$created)
	if ("last_access" %in% names(df)) df$last_access <- as.POSIXct(df$last_access)
	if ("ended" %in% names(df)) df$ended <- as.POSIXct(df$ended)
	if ("testing" %in% names(df)) df$testing <- as.logical(df$testing)
	if ("position" %in% names(df)) df$position <- as.integer(df$position)
	
	# 3. Flatten 'current_unit' if it exists
	# The API returns nested objects: "current_unit": {"id": 1, ...}
	if ("current_unit" %in% names(df)) {
		
		# Helper to safely extract a field from the list-column
		extract_field <- function(lst_col, field) {
			sapply(lst_col, function(x) {
				if (is.null(x) || !is.list(x) || is.null(x[[field]])) return(NA)
				return(x[[field]])
			})
		}
		
		df$unit_id <- as.integer(extract_field(df$current_unit, "id"))
		df$unit_type <- as.character(extract_field(df$current_unit, "type"))
		df$unit_description <- as.character(extract_field(df$current_unit, "description"))
		df$unit_session_id <- as.integer(extract_field(df$current_unit, "session_id"))
		
		# Remove the original nested list column
		df$current_unit <- NULL
	}
	
	return(dplyr::as_tibble(df))
}

#' List Per-Unit Sessions in a Run
#'
#' Returns one row per (participant × unit × iteration) for the run — the
#' history view that complements [formr_api_sessions()] (which gives one
#' row per participant with their *current* unit only).
#'
#' Use this for trajectory plots (Sankey, alluvial), drop-off analytics,
#' and debugging stuck participants. The rows arrive ordered by
#' `(session, created, unit_session_id)`, so `dplyr::group_by(session) |>
#' dplyr::mutate(next_unit = dplyr::lead(unit_description))` gives the
#' edges of a trajectory plot directly.
#'
#' Special units (OverviewScriptPage, ServiceMessagePage, ReminderEmail)
#' surface with `position = NA` because they live outside the ordered
#' run flow.
#'
#' @param run_name Name of the run.
#' @param session_codes Optional character vector — restrict to one or
#'   more participants' histories.
#' @param testing Filter: TRUE for test sessions only, FALSE for real
#'   participants only, NULL for both.
#' @param since Optional ISO 8601 datetime string. Returns only unit
#'   sessions whose `created` is at-or-after this — handy for
#'   incremental polling.
#' @param limit Pagination limit (default 1000, max 10000).
#' @param offset Pagination offset (default 0).
#' @return A tidy tibble with columns: `unit_session_id`, `session`,
#'   `testing`, `unit_id`, `unit_type`, `unit_description`, `position`,
#'   `iteration`, `created`, `expires`, `ended`, `expired`, `result`,
#'   `state`.
#' @export
formr_api_unit_sessions <- function(run_name, session_codes = NULL, testing = NULL,
                                     since = NULL, limit = 1000, offset = 0) {
	query <- list(limit = limit, offset = offset)
	if (!is.null(session_codes)) {
		# The API accepts comma-delimited codes; collapse here so the
		# caller gets a vector-in / vector-out experience.
		query$session <- paste(session_codes, collapse = ",")
	}
	if (!is.null(testing)) query$testing <- if (testing) 1 else 0
	if (!is.null(since))   query$since   <- since

	res <- formr_api_request(paste0("runs/", run_name, "/unit_sessions"), query = query)

	if (length(res) == 0) {
		message(sprintf("[INFO] No unit sessions found for run '%s'.", run_name))
		return(dplyr::tibble())
	}

	df <- dplyr::bind_rows(res)

	# Type coercion — JSON gives numbers and strings, R wants typed columns.
	if ("created" %in% names(df))  df$created  <- as.POSIXct(df$created)
	if ("expires" %in% names(df))  df$expires  <- as.POSIXct(df$expires)
	if ("ended"   %in% names(df))  df$ended    <- as.POSIXct(df$ended)
	if ("expired" %in% names(df))  df$expired  <- as.POSIXct(df$expired)
	if ("testing" %in% names(df))  df$testing  <- as.logical(df$testing)
	if ("position" %in% names(df)) df$position <- as.integer(df$position)
	if ("iteration" %in% names(df)) df$iteration <- as.integer(df$iteration)

	dplyr::as_tibble(df)
}