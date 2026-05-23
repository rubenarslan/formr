#' List files attached to a run
#' 
#' Returns a data frame of all files uploaded to a specific run, including their 
#' public URLs and timestamps.
#' 
#' @param run_name Name of the run.
#' @return A data.frame containing: id, name, path, url, created, modified.
#' @importFrom dplyr bind_rows
#' @export
formr_api_files <- function(run_name) {
	# GET /runs/{run_name}/files
	res <- formr_api_request(paste0("runs/", run_name, "/files"), method = "GET")
	
	if (length(res) == 0) {
		message(sprintf("[INFO] No files found for run '%s'.", run_name))
		return(data.frame())
	}
	
	# Use bind_rows to handle both list-of-lists and simplified data.frames
	df <- dplyr::bind_rows(res)
	
	# Robust timestamp conversion: Only convert if the columns exist
	if ("created" %in% names(df)) {
		df$created <- as.POSIXct(df$created)
	}
	
	if ("modified" %in% names(df)) {
		df$modified <- as.POSIXct(df$modified)
	}
	
	df
}

#' Upload File(s) to Run
#' 
#' Uploads local file(s) to the run. Accepts a single file path, a vector of file paths,
#' or a directory path (which will upload all files within that directory).
#' 
#' @param run_name Name of the run.
#' @param path Local path to the file, a vector of paths, or a directory path.
#' @return Invisibly returns a list of server responses.
#' @export
formr_api_upload_file <- function(run_name, path) {
	
	# 1. Handle Directory Input
	# If a single path is provided and it is a directory, get all files inside
	if (length(path) == 1 && dir.exists(path)) {
		message(sprintf("[INFO] Directory detected. Preparing to upload files from '%s'...", path))
		path <- list.files(path, full.names = TRUE, recursive = FALSE)
		
		if (length(path) == 0) {
			warning("Directory is empty.")
			return(invisible(NULL))
		}
	}
	
	# 2. Iterate through vector of paths
	results <- lapply(path, function(single_path) {
		if (!file.exists(single_path)) {
			warning(sprintf("Skipping: File not found: %s", single_path))
			return(NULL)
		}
		
		# POST /runs/{run_name}/files
		# Server expects 'file' key in multipart/form-data
		body <- list(file = httr::upload_file(single_path))
		
		res <- formr_api_request(
			endpoint = paste0("runs/", run_name, "/files"), 
			method = "POST", 
			body = body
		)
		
		message(sprintf("[SUCCESS] File '%s' uploaded successfully.", res$file))
		return(res)
	})
	
	invisible(results)
}

#' Delete file(s) from a run
#' 
#' Removes file attachment(s) from the run. Accepts a single filename, a vector
#' of filenames, or a local directory path (which will delete files on the server
#' that match the names of the files in the local directory).
#' 
#' @param run_name Name of the run.
#' @param file_name The name of the file(s) to delete (e.g. "image.png"), or a local directory path.
#' @export
formr_api_delete_file <- function(run_name, file_name) {
	
	# 1. Handle Directory Input
	# If input is a directory, we assume user wants to delete files on server
	# that match the filenames found in that local directory.
	if (length(file_name) == 1 && dir.exists(file_name)) {
		message(sprintf("[INFO] Directory detected. Deleting files matching names in '%s'...", file_name))
		# We only need the basename (e.g. "image.png") not the full local path
		file_name <- basename(list.files(file_name, full.names = TRUE, recursive = FALSE))
		
		if (length(file_name) == 0) {
			warning("Directory is empty. No files to delete.")
			return(invisible(NULL))
		}
	}
	
	# 2. Iterate through vector of filenames
	lapply(file_name, function(single_name) {
		
		# DELETE /runs/{run_name}/files/{file_name}
		encoded_name <- utils::URLencode(single_name)
		
		tryCatch({
			formr_api_request(
				endpoint = paste0("runs/", run_name, "/files/", encoded_name),
				method = "DELETE"
			)
			message(sprintf("[SUCCESS] File '%s' deleted.", single_name))
		}, error = function(e) {
			warning(sprintf("[FAILED] Failed to delete '%s': %s", single_name, e$message))
		})
	})
	
	invisible(TRUE)
}

#' Delete ALL files attached to a run
#'
#' CAUTION: This will permanently remove every file attached to the specified run.
#' It first fetches the list of existing files, then iterates through them to delete.
#'
#' @param run_name Name of the run.
#' @param prompt Logical. If TRUE (default), the function asks for interactive confirmation 
#'        before deleting. Set to FALSE for automated scripts (use with care).
#' @export
formr_api_delete_all_files <- function(run_name, prompt = TRUE) {
	# 1. Fetch existing files
	files <- formr_api_files(run_name)
	
	if (nrow(files) == 0) {
		message(sprintf("[INFO] No files found in run '%s'. Nothing to delete.", run_name))
		return(invisible(TRUE))
	}
	
	file_names <- files$name
	count <- length(file_names)
	
	# 2. Safety Prompt
	if (prompt) {
		cat(sprintf("DANGER: You are about to delete %d files from run '%s'.\n", count, run_name))
		cat("Files: ", paste(head(file_names, 3), collapse = ", "), if(count > 3) "..." else "", "\n")
		
		response <- readline(prompt = "Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("[FAILED] Operation cancelled.")
			return(invisible(FALSE))
		}
	}
	
	# 3. Perform Deletion
	# Since we updated formr_api_delete_file to accept vectors, we can pass the whole list.
	message(sprintf("[START] Starting deletion of %d files...", count))
	formr_api_delete_file(run_name, file_names)
	
	message("[SUCCESS] All files have been processed.")
	invisible(TRUE)
}