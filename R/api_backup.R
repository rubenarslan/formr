#' Backup a study
#'
#' Downloads the full run structure, all survey items, attached files, and results.
#' Saves everything into a structured folder.
#'
#' @param run_name Name of the run/study.
#' @param dir Directory to write the backup into. Defaults to a sub-folder named
#'   after the run inside [formr_default_dir()]; set that (or pass `dir`) since
#'   formr never writes to the working directory by default.
#' @param prompt Logical. If TRUE (default), asks for confirmation before
#'   overwriting when run interactively.
#' @param verbose Logical. If TRUE (default), reports progress via [message()].
#' @return Invisibly `NULL`; called for its side effect of writing the run
#'   structure (JSON), surveys, files and results (`results.rds`) into `dir`.
#' @export
#' @examples
#' \dontrun{
#' # Not run: needs a live formr server and an authenticated session.
#' formr_api_backup_run("my_run", dir = tempdir())
#' }
formr_api_backup_run <- function(run_name, dir = NULL, prompt = TRUE, verbose = TRUE) {

	if (is.null(dir)) dir <- file.path(.formr_default_or_stop("dir"), run_name)

	if (prompt && interactive()) {
		warning(sprintf(
			"You are about to overwrite local files in '%s' with data from run '%s'. Any local changes will be LOST.",
			normalizePath(dir, mustWork = FALSE), run_name), call. = FALSE, immediate. = TRUE)
		response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("Operation cancelled.")
			return(invisible(NULL))
		}
	}

	# Directory creation & safety check
	if (!dir.exists(dir)) {
		created <- dir.create(dir, showWarnings = TRUE, recursive = TRUE)
		if (!created) {
			stop(sprintf("Could not create directory '%s'. Check your permissions or path.", dir))
		}
	}

	if (verbose) message(sprintf("Backing up study '%s' to '%s'...", run_name, dir))

	# Run structure (JSON)
	tryCatch({
		struct <- formr_api_run_structure(run_name)
		jsonlite::write_json(struct, file.path(dir, "run_structure.json"), pretty = TRUE, auto_unbox = TRUE)
		.sync_api_server_surveys(struct, dir = dir)
	}, error = function(e) warning("Failed to download run structure: ", e$message))

	# Files
	.sync_api_server_files(run_name, dir = dir)

	# Results
	if (verbose) message("Downloading results...")
	tryCatch({
		results <- formr_api_results(run_name)
		saveRDS(results, file = file.path(dir, "results.rds"))
		if (verbose) message("Results saved to results.rds")
	}, error = function(e) warning("Failed to download results: ", e$message))

	if (verbose) message("Backup complete.")
	invisible(NULL)
}
