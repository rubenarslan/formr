#' List all runs
#' 
#' Returns a data frame of all runs accessible to the user, including status flags
#' and timestamps.
#' 
#' @return A data.frame containing run details: id, name, title, public (bool),
#' cron_active (bool), locked (bool), created (POSIXct), modified (POSIXct).
#' @importFrom dplyr bind_rows mutate
#' @export
formr_api_runs <- function() {
	raw <- formr_api_request("runs", api_version = "v1")
	
	if (length(raw) == 0) return(data.frame())
	
	dplyr::bind_rows(raw) %>%
		dplyr::mutate(
			created = as.POSIXct(.data$created),
			modified = as.POSIXct(.data$modified),
			# Convert 0/1 integers to logical booleans for clarity
			public = as.logical(.data$public),
			cron_active = as.logical(.data$cron_active),
			locked = as.logical(.data$locked)
		)
}

#' Create a new run
#' 
#' Creates one or more new runs on the server. Prints a confirmation message 
#' with the public link for each.
#' 
#' @param name A character vector of names for the new runs (must be unique).
#' @param verbose Logical. If TRUE (default), reports progress via [message()].
#' @return Invisibly returns a data frame containing the `name` and `link` of the created runs.
#' @importFrom dplyr bind_rows
#' @export
formr_api_create_run <- function(name, verbose = TRUE) {

	# Helper function to create a single run
	create_single <- function(single_name) {
		# Endpoint: POST /v1/runs/{name}
		res <- formr_api_request(paste0("runs/", single_name), method = "POST", api_version = "v1")

		# User-friendly feedback
		if (verbose) message(sprintf("Success! Run '%s' created.", res$name))
		if (verbose) message(sprintf(" Link: %s", res$link))
		
		return(res)
	}
	
	# Iterate over all provided names
	results_list <- lapply(name, create_single)
	
	# Combine results into a tidy data frame
	results_df <- dplyr::bind_rows(results_list)
	
	# Return the result invisibly so it can be assigned if needed
	invisible(results_df)
}

#' Get or Update Run Settings
#' 
#' Retrieve the settings for one or more runs as a tidy data frame, or update them
#' by providing a named list of new values.
#' 
#' @param run_name Name of the run (or a vector of names).
#' @param settings A list of settings to update (e.g., `list(public = 1, locked = TRUE)`). 
#'   If NULL, returns the current settings.
#' @param verbose Logical. If TRUE (default), reports progress via [message()].
#' @return
#'   - If `settings` is NULL: A data.frame/tibble with details for all requested runs.
#'   - If `settings` is provided: Invisibly returns TRUE on success.
#' @importFrom dplyr bind_rows mutate
#' @export
formr_api_run_settings <- function(run_name, settings = NULL, verbose = TRUE) {
	
	# --- Helper function to process a single run ---
	process_single_run <- function(single_name) {
		if (is.null(settings)) {
			# GET: Fetch and clean
			raw <- formr_api_request(paste0("runs/", single_name), api_version = "v1")
			
			# Convert to tibble
			df <- dplyr::bind_rows(raw) %>%
				dplyr::mutate(
					created = as.POSIXct(.data$created),
					modified = as.POSIXct(.data$modified),
					locked = as.logical(.data$locked),
					cron_active = as.logical(.data$cron_active),
					use_material_design = as.logical(.data$use_material_design),
					public = as.integer(.data$public)
				)
			return(df)
			
		} else {
			# PATCH: Update settings
			formr_api_request(paste0("runs/", single_name), method = "PATCH", body = settings, api_version = "v1")
			if (verbose) message(sprintf("Settings updated successfully for run '%s'.", single_name))
			return(NULL)
		}
	}
	
	# --- Main Logic: Iterate over all provided names ---
	
	if (is.null(settings)) {
		# GET Mode: Apply function to all names and stack the resulting dataframes
		all_runs_data <- dplyr::bind_rows(lapply(run_name, process_single_run))
		return(all_runs_data)
		
	} else {
		# PATCH Mode: Apply function to all names (output is printed messages)
		# We use invisible() to suppress the list of NULLs returned by lapply
		invisible(lapply(run_name, process_single_run))
		return(invisible(TRUE))
	}
}

#' Get or Update Run Structure (Run Units)
#' 
#' Export the current run structure as a list (GET) or replace it by importing 
#' a JSON file (PUT).
#' 
#' @param run_name Name of the run.
#' @param structure_json_path Optional path to a JSON file to IMPORT (PUT) structure. 
#'   If provided, the function uploads this file to the server.
#' @param file Optional path to save the DOWNLOADED (GET) structure as a .json file.
#'   This ensures a perfect 1:1 backup of the server configuration.
#' @param verbose Logical. If TRUE (default), reports progress via [message()].
#' @return
#'   - GET (default): A `formr_run_structure` object (list) for inspection.
#'   - GET (file provided): Invisibly returns the file path.
#'   - PUT: Invisibly returns TRUE on success.
#' @export
formr_api_run_structure <- function(run_name, structure_json_path = NULL, file = NULL, verbose = TRUE) {
	
	# --- PUT Mode: Import/Upload Structure ---
	if (!is.null(structure_json_path)) {
		if (!file.exists(structure_json_path)) {
			stop("File not found: ", structure_json_path)
		}
		
		# Read and Validate JSON locally
		json_content <- paste(readLines(structure_json_path, warn = FALSE), collapse = "\n")
		
		if (!jsonlite::validate(json_content)) {
			stop("The provided file contains invalid JSON. Please check syntax.")
		}
		
		# Send PUT Request
		res <- formr_api_request(
			endpoint = paste0("runs/", run_name, "/structure"),
			method = "PUT",
			body = json_content,
			encode = "raw"
		)
		
		if (verbose) message("Structure imported successfully.")
		return(invisible(TRUE))
	}
	
	# --- GET Mode: Export/Download Structure ---
	
	# Option A: User wants to save to file (Backup)
	# We fetch RAW text to ensure 1:1 fidelity (avoiding R list <-> JSON conversion artifacts)
	if (!is.null(file)) {
		session <- formr_api_session()
		url <- session$base_url
		url$path <- paste0(url$path, "/v1/runs/", run_name, "/structure")
		
		# Manual request to get raw text
		response <- httr::GET(
			url, 
			httr::add_headers(Authorization = paste("Bearer", session$token))
		)
		httr::stop_for_status(response)
		
		# Write raw content directly to file
		writeBin(httr::content(response, "raw"), file)
		if (verbose) message(sprintf("Backup saved to: %s", file))
		return(invisible(file))
	}
	
	# Option B: User wants to inspect in R (Interactive)
	res <- formr_api_request(
		endpoint = paste0("runs/", run_name, "/structure"), 
		method = "GET"
	)
	
	# Add S3 class for pretty printing
	class(res) <- c("formr_run_structure", class(res))
	return(res)
}

#' Print method for formr run structure
#' @param x The object.
#' @param ... Additional arguments.
#' @return Invisibly returns `x`; called for its side effect of printing a formatted table of the run's units.
#' @export
print.formr_api_run_structure <- function(x, ...) {
	cat(sprintf(" Run Structure: %s\n", x$name))
	cat(sprintf("   (Total Units: %d)\n\n", length(x$units)))
	
	if (length(x$units) == 0) {
		cat("   [Empty Run]\n")
		return(invisible(x))
	}
	
	df <- as.data.frame(x)
	
	# Dynamic column width
	w_pos <- max(nchar(as.character(df$position)), 3)
	w_type <- max(nchar(as.character(df$type)), 4)
	
	# Header
	cat(sprintf(paste0("%-", w_pos, "s | %-", w_type, "s | %s\n"), "Pos", "Type", "Details"))
	cat(paste(rep("-", w_pos + w_type + 20), collapse = ""), "\n")
	
	# Rows
	for(i in 1:nrow(df)) {
		cat(sprintf(paste0("%0", w_pos, "d | %-", w_type, "s | %s\n"), 
								df$position[i], 
								df$type[i], 
								df$details[i]))
	}
	invisible(x)
}

#' Convert formr run structure to data.frame
#' @param x The object.
#' @param ... Additional arguments.
#' @return A data.frame with one row per unit and columns `position`, `type`, `description` and `details`.
#' @export
as.data.frame.formr_api_run_structure <- function(x, ...) {
	if (length(x$units) == 0) return(data.frame())
	
	do.call(rbind, lapply(x$units, function(u) {
		details <- ""
		
		if (u$type == "Survey") {
			n_items <- if(!is.null(u$survey_data$items)) length(u$survey_data$items) else 0
			details <- sprintf("%s (%d items)", u$survey_data$name, n_items)
		} else if (u$type %in% c("SkipForward", "SkipBackward")) {
			target <- if(!is.null(u$if_true)) u$if_true else "End"
			details <- sprintf("Jump to Pos %s", target)
		} else if (u$type == "Email") {
			details <- sprintf("Subject: %s", u$subject)
		} else if (u$type == "Pause") {
			details <- sprintf("Wait: %s", u$wait_minutes)
		} else if (u$type == "Endpage") {
			body_preview <- substr(u$body, 1, 30)
			if(nchar(u$body) > 30) body_preview <- paste0(body_preview, "...")
			details <- body_preview
		}
		
		data.frame(
			position = as.integer(u$position),
			type = u$type,
			description = if(is.null(u$description)) "" else u$description,
			details = details,
			stringsAsFactors = FALSE
		)
	})) -> df
	
	df[order(df$position), ]
}

#' Delete a Run
#'
#' Permanently deletes a run and all associated data (sessions, results).
#'
#' @param run_name Name of the run to delete.
#' @param prompt Logical. If TRUE (default), asks for interactive confirmation.
#' @param verbose Logical. If TRUE (default), reports progress via [message()].
#' @return Invisibly `TRUE` (single run) or a named logical vector (multiple
#'   runs) indicating per-run success; `FALSE` if the user declines the prompt.
#' @export
formr_api_delete_run <- function(run_name, prompt = TRUE, verbose = TRUE) {
	
	if (length(run_name) > 1) {
		if (prompt && interactive()) {
			warning(sprintf(
				"You are about to permanently delete %d runs (%s). This includes ALL structure and attached files and cannot be undone.",
				length(run_name), paste(run_name, collapse = ", ")), call. = FALSE, immediate. = TRUE)
			response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
			if (tolower(trimws(response)) != "y") {
				message("Operation cancelled.")
				return(invisible(FALSE))
			}
		}
		results <- vapply(run_name, function(rn) {
			tryCatch({
				formr_api_request(endpoint = paste0("runs/", rn), method = "DELETE")
				if (verbose) message(sprintf("Run '%s' deleted.", rn))
				TRUE
			}, error = function(e) {
				warning(sprintf("Run '%s': %s", rn, e$message), call. = FALSE)
				FALSE
			})
		}, logical(1))
		successes <- sum(results)
		failures <- sum(!results)
		if (failures > 0) {
			warning(sprintf("%d run(s) deleted, %d failed.", successes, failures), call. = FALSE)
		} else if (verbose) {
			message(sprintf("%d run(s) deleted.", successes))
		}
		return(invisible(results))
	}
	
	if (prompt && interactive()) {
		warning(sprintf(
			"You are about to permanently delete the run '%s'. This includes ALL structure and attached files and cannot be undone.",
			run_name), call. = FALSE, immediate. = TRUE)
		response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("Operation cancelled.")
			return(invisible(FALSE))
		}
	}

	tryCatch({
		# DELETE /runs/{run_name}
		formr_api_request(
			endpoint = paste0("runs/", run_name),
			method = "DELETE"
		)
		if (verbose) message(sprintf("Run '%s' deleted.", run_name))
		return(invisible(TRUE))
		
	}, error = function(e) {
		stop(sprintf("Failed to delete run '%s': %s", run_name, e$message))
	})
}