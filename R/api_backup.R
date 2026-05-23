#' Backup a study
#'
#' Downloads the full run structure, all survey items, attached files, and results.
#' Saves everything into a structured folder.
#'
#' @param run_name Name of the run/study.
#' @param dir Local folder to save data (defaults to run_name).
#' @param prompt Logical. If TRUE (default), asks for confirmation before overwriting.
#' @export
formr_api_backup_run <- function(run_name, dir = NULL, prompt = TRUE) {
	
	if (prompt) {
		cat(sprintf("[WARNING]  WARNING: You are about to overwrite local files in '%s' with data from run '%s'.\n", normalizePath(dir, mustWork = FALSE), run_name))
		cat("   Any local changes will be LOST.\n")
		
		response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("[FAILED] Operation cancelled.")
			return(invisible(FALSE))
		}
	}
	
	if (is.null(dir)) dir <- run_name
	
	# 1. Directory Creation & Safety Check
	if (!dir.exists(dir)) {
		created <- dir.create(dir, showWarnings = TRUE, recursive = TRUE)
		if (!created) {
			stop(sprintf("CRITICAL ERROR: Could not create directory '%s'. Check your permissions or path.", dir))
		}
	}
	
	message(sprintf(" Backing up study '%s' to '%s'...", run_name, dir))
	
	# 2. Run Structure (JSON)
	tryCatch({
		struct <- formr_api_run_structure(run_name)
		jsonlite::write_json(struct, file.path(dir, "run_structure.json"), pretty = TRUE, auto_unbox = TRUE)
		
		# 3. Surveys (Using Shared Helper)
		.sync_api_server_surveys(struct, dir = dir)
		
	}, error = function(e) warning("Failed to download run structure: ", e$message))
	
	# 4. Files (Using Shared Helper)
	.sync_api_server_files(run_name, dir = dir)
	
	# 5. Results
	message("  Downloading results...")
	tryCatch({
		results <- formr_api_results(run_name)
		saveRDS(results, file = file.path(dir, "results.rds"))
		message("   [SUCCESS] Results saved to results.rds")
	}, error = function(e) warning("Failed to download results: ", e$message))
	
	message("[SUCCESS] Backup complete.")
}