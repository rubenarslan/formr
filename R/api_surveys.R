#' List Surveys
#' 
#' Returns a list of all surveys owned by the user.
#' 
#' @param name_pattern Optional. Filter surveys by name (partial match).
#' @return A tibble of surveys (id, name, created, modified, results_table).
#' @importFrom dplyr bind_rows as_tibble mutate
#' @export
formr_api_surveys <- function(name_pattern = NULL) {
	query <- list()
	if (!is.null(name_pattern)) query$name <- name_pattern
	
	# Source: ApiHelperV1.php surveys (GET)
	res <- formr_api_request("surveys", query = query)
	
	if (length(res) == 0) {
		message("[INFO] No surveys found.")
		return(dplyr::tibble())
	}
	
	dplyr::bind_rows(res) %>% 
		dplyr::mutate(
			created = as.POSIXct(.data$created),
			modified = as.POSIXct(.data$modified),
			id = as.integer(.data$id)
		) %>%
		dplyr::as_tibble()
}

#' Get Survey Structure (Items)
#' 
#' Retrieves the item table for a survey. Can return a tibble (JSON) or download
#' the original Excel file (XLSX).
#' 
#' @param survey_name The name of the survey.
#' @param format The format to retrieve: "json" (default) or "xlsx".
#' @param file_path Optional. Required if format is "xlsx".
#' @export
formr_api_survey_structure <- function(survey_name, format = "json", file_path = NULL) {
	
	# --- Mode: Download XLSX ---
	if (format == "xlsx") {
		if (is.null(file_path)) stop("You must provide a 'file_path' when format is 'xlsx'.")
		
		# 1. Get Session (Using your existing API core)
		session <- formr_api_session()
		if (is.null(session)) stop("Not authenticated. Run formr_api_authenticate() first.")
		
		# 2. Construct URL (mimicking formr_api_request logic)
		url <- session$base_url
		# Append path: /v1/surveys/{name}
		# We assume api_version is v1 as per your core code
		url$path <- paste0(url$path, "/v1/surveys/", survey_name)
		url$path <- gsub("//", "/", url$path) # Clean up double slashes
		
		tryCatch({
			# 3. Direct HTTR call to save binary file
			res <- httr::GET(
				url, # httr accepts the list object directly
				query = list(format = "xlsx"),
				httr::add_headers(Authorization = paste("Bearer", session$token)),
				httr::write_disk(file_path, overwrite = TRUE)
			)
			
			# 4. Check for API Errors (e.g. 404, 403)
			if (httr::status_code(res) >= 400) {
				if(file.exists(file_path)) file.remove(file_path) # Delete empty/error file
				# Try to read error message if possible
				err_msg <- tryCatch(httr::content(res, "text", encoding="UTF-8"), error=function(e) "Unknown error")
				stop(sprintf("API Error (%s): %s", httr::status_code(res), substr(err_msg, 1, 100)))
			}
			
			message(sprintf("   - Downloaded: %s", basename(file_path)))
			return(invisible(file_path))
			
		}, error = function(e) {
			# Clean up file if download crashed halfway
			if(file.exists(file_path) && file.info(file_path)$size == 0) file.remove(file_path)
			warning(sprintf("Failed to download XLSX for '%s': %s", survey_name, e$message))
			return(NULL)
		})
	}
	
	# --- Mode: Fetch JSON (Your existing logic) ---
	res <- formr_api_request(endpoint = paste0("surveys/", survey_name), method = "GET")
	
	if (is.null(res$items) || length(res$items) == 0) {
		warning(sprintf("[WARNING] Survey '%s' exists but has no items.", survey_name))
		return(dplyr::tibble())
	}
	
	process_item <- function(x) {
		complex_fields <- c("input_attributes", "parent_attributes", "allowed_classes", "choices", "val_errors", "val_warnings")
		for (field in complex_fields) {
			if (!is.null(x[[field]])) x[[field]] <- list(x[[field]]) else x[[field]] <- list(NULL) 
		}
		if (!is.null(x$value)) x$value <- as.character(x$value)
		if (!is.null(x$type_options)) x$type_options <- as.character(x$type_options)
		x <- lapply(x, function(val) if (is.null(val)) NA else val)
		return(tibble::as_tibble(x))
	}
	
	item_list <- unname(res$items)
	clean_list <- lapply(item_list, process_item)
	return(dplyr::bind_rows(clean_list))
}

#' Upload/Update Survey
#' 
#' Uploads a survey structure.
#' 
#' @param file_path Path to a local file.
#' @param google_sheet_url Google Sheet URL.
#' @export
formr_api_upload_survey <- function(file_path = NULL, google_sheet_url = NULL) {
	
	body <- list()
	
	if (!is.null(google_sheet_url)) {
		body$google_sheet <- google_sheet_url
	} else if (!is.null(file_path)) {
		if (!file.exists(file_path)) stop("File not found: ", file_path)
		body$file <- httr::upload_file(file_path)
	} else {
		stop("You must provide either 'file_path' or 'google_sheet_url'.")
	}
	
	# POST to /surveys (collection) to create or update based on filename
	res <- formr_api_request(
		endpoint = "surveys",
		method = "POST",
		body = body,
		encode = "multipart" 
	)
	
	message(sprintf("[SUCCESS] Survey '%s' processed successfully.", res$name))
	
	if (!is.null(res$logs) && length(res$logs) > 0) {
		cat("\n[INFO] Server Logs:\n")
		logs <- unlist(res$logs)
		clean_logs <- gsub("<[^>]+>", "", logs)
		cat(paste0("...", clean_logs, collapse = "\n"), "\n")
	}
	
	invisible(res)
}

#' Delete a Survey
#'
#' Permanently deletes a survey study.
#' Note: The API may prevent deletion if this survey is currently used in an active run.
#'
#' @param survey_name Name of the survey to delete.
#' @param prompt Logical. If TRUE (default), asks for interactive confirmation.
#' @return Invisibly returns TRUE on success.
#' @export
formr_api_delete_survey <- function(survey_name, prompt = TRUE) {
	
	if (prompt) {
		cat(sprintf("\n[WARNING]  WARNING: You are about to delete the survey '%s'.\n", survey_name))
		
		response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("[FAILED] Operation cancelled.")
			return(invisible(FALSE))
		}
	}
	
	tryCatch({
		# DELETE /surveys/{survey_name}
		formr_api_request(
			endpoint = paste0("surveys/", survey_name), 
			method = "DELETE"
		)
		message(sprintf("[SUCCESS] Survey '%s' deleted successfully.", survey_name))
		return(invisible(TRUE))
		
	}, error = function(e) {
		stop(sprintf("Failed to delete survey '%s': %s", survey_name, e$message))
	})
}