#' Pull Project from Server
#' Scaffolds folder structure if missing, then overwrites local files with Server state.
#' @param run_name Name of the run.
#' @param dir Local directory (default ".").
#' @param prompt Logical. If TRUE (default), asks for confirmation before overwriting (unless dir is empty).
#' @export
formr_api_pull_project <- function(run_name, dir = ".", prompt = TRUE) {
	
	# --- 1. Scaffolding ---
	
	# Ensure main directory exists
	if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
	
	# Ensure sub-directories exist
	dirs <- c("surveys", "files", "css", "js")
	for (d in dirs) {
		sub_d <- file.path(dir, d)
		if (!dir.exists(sub_d)) dir.create(sub_d, recursive = TRUE)
	}
	
	# Ensure .formrignore exists
	if (!file.exists(file.path(dir, ".formrignore"))) {
		writeLines(c(".git", ".Rproj.user", "*.Rproj", ".DS_Store", "backup/", "results.rds"), file.path(dir, ".formrignore"))
	}
	
	# --- 2. Safety Check ---
	
	# If the directory is effectively empty, we skip warning the user about overwriting.
	is_fresh_install <- length(list.files(dir, pattern = "\\.json$|surveys|files", recursive = TRUE)) == 0
	
	if (prompt && !is_fresh_install) {
		cat(sprintf("[WARNING] You are about to overwrite local files in '%s' with data from run '%s'.\n", normalizePath(dir, mustWork = FALSE), run_name))
		cat("   Any local changes that have not been pushed to the server will be LOST.\n")
		
		response <- readline(prompt = "   Are you sure you want to proceed? (y/n): ")
		if (tolower(trimws(response)) != "y") {
			message("[FAILED] Operation cancelled.")
			return(invisible(FALSE))
		}
	}
	
	message("[INFO] Pulling changes from Server...")
	
	struct <- NULL
	
	# --- 3. Structure ---
	tryCatch({
		struct <- formr_api_run_structure(run_name)
		jsonlite::write_json(struct, file.path(dir, "run_structure.json"), pretty = TRUE, auto_unbox = TRUE)
		message("[SUCCESS] Structure downloaded.")
	}, error = function(e) warning("Failed to pull structure: ", e$message))
	
	# --- 4. Settings ---
	tryCatch({
		settings <- formr_api_run_settings(run_name)
		
		# CSS/JS extraction
		if (!is.null(settings$custom_css)) {
			writeLines(settings$custom_css, file.path(dir, "css", "custom.css")); settings$custom_css <- "" 
		}
		if (!is.null(settings$custom_js)) {
			writeLines(settings$custom_js, file.path(dir, "js", "custom.js")); settings$custom_js <- ""
		}
		
		# Cleanup
		read_only <- c("id", "link", "created", "modified", "json_jwt")
		for (ro in read_only) settings[[ro]] <- NULL
		jsonlite::write_json(settings, file.path(dir, "run_settings.json"), pretty = TRUE, auto_unbox = TRUE)
		message("[SUCCESS] Settings downloaded.")
	}, error = function(e) warning("Failed to pull settings: ", e$message))
	
	# --- 5. Surveys (Using Shared Helper) ---
	if (!is.null(struct)) {
		.sync_api_server_surveys(struct, dir)
	}
	
	# --- 6. Files (Using Shared Helper) ---
	.sync_api_server_files(run_name, dir)
	
	message("[SUCCESS] Project files updated from server.")
}

#' Push Project to Server
#' 
#' Uploads local project files (surveys, assets, settings) to the formr server.
#' Optionally monitors the directory for subsequent changes (Watcher mode).
#'
#' @param run_name Name of the run.
#' @param dir Local directory (default ".").
#' @param watch Logical. If TRUE, keeps the connection open and uploads changes immediately when files are saved.
#' @param background Logical. If TRUE (default), launches watcher as an RStudio Job.
#' @param interval Seconds between checks (default 2).
#' @export
formr_api_push_project <- function(run_name, dir = ".", watch = FALSE, background = TRUE, interval = 2) {
	if (!dir.exists(dir)) stop("Directory not found. Run formr_api_pull_project() first.")
	
	# 1. Initial Push (Sync current state immediately)
	# We do this in the main thread so the user sees immediate success/failure
	message(sprintf("[INFO] Pushing '%s' -> Run '%s'", normalizePath(dir), run_name))
	current_state <- get_api_project_state(dir)
	initial_changes <- list(added = names(current_state), modified = character(0), deleted = character(0))
	
	if (length(initial_changes$added) > 0) {
		handle_api_project_changes(run_name, dir, initial_changes)
	} else {
		message("[INFO] No initial changes to push.")
	}
	
	# 2. Watcher Logic
	if (watch) {
		# Check if we should launch a Background Job
		if (background && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
			
			# Capture current authentication to pass to the background job
			session <- formr_api_session()
			auth_cmd <- ""
			if (!is.null(session) && !is.null(session$token)) {
				# Reconstruct host URL from parsed list
				host_url <- httr::build_url(session$base_url)
				auth_cmd <- sprintf('formr::formr_api_authenticate(host = "%s", access_token = "%s")', 
														host_url, session$token)
			}
			
			# Create a self-contained script for the job
			job_script <- tempfile(pattern = "formr_watch_", fileext = ".R")
			script_content <- c(
				"library(formr)",
				auth_cmd, # Inject authentication
				sprintf("setwd('%s')", getwd()), # Ensure job runs in correct WD
				sprintf("message('Starting Watcher for run: %s')", run_name),
				# Call recursively with background=FALSE to enter the actual loop
				sprintf("formr::formr_api_push_project(run_name = '%s', dir = '%s', watch = TRUE, background = FALSE, interval = %d)", 
								run_name, dir, interval)
			)
			writeLines(script_content, job_script)
			
			# Launch the job
			rstudioapi::jobRunScript(
				path = job_script,
				name = paste0("Sync: ", run_name),
				workingDir = getwd(),
				importEnv = FALSE 
			)
			
			message("[SUCCESS] Watcher started in the 'Jobs' tab.")
			return(invisible(TRUE))
		}
		
		# --- Blocking Loop (Runs if background=FALSE or not in RStudio) ---
		message(sprintf("\n[INFO] Watching '%s' for changes (Press Esc to stop)...", dir))
		
		last_state <- current_state
		
		tryCatch({
			while (TRUE) {
				Sys.sleep(interval)
				
				# Detect changes
				new_state <- get_api_project_state(dir)
				changes <- detect_api_changes(last_state, new_state)
				
				if (length(changes$added) > 0 || length(changes$modified) > 0 || length(changes$deleted) > 0) {
					# Print timestamp for log clarity
					message(sprintf("\n[%s] Change detected...", format(Sys.time(), "%H:%M:%S")))
					
					# Handle Sync
					handle_api_project_changes(run_name, dir, changes)
					
					# Update State
					last_state <- new_state
				}
			}
		}, interrupt = function(i) {
			message("\n[INFO] Watcher stopped.")
		})
	}
}

# --- INTERNAL HELPERS ---

#' Get recursive file state
#' @noRd
get_api_project_state <- function(dir) {
	all_files <- list.files(dir, recursive = TRUE, full.names = FALSE, all.files = TRUE)
	ignores <- read_ignore_file(dir)
	files_to_track <- Filter(function(f) !is_ignored(f, ignores), all_files)
	info <- file.info(file.path(dir, files_to_track))
	structure(as.numeric(info$mtime), names = files_to_track)
}

#' Read .formrignore
#' @noRd
read_ignore_file <- function(dir) {
	f <- file.path(dir, ".formrignore")
	if (file.exists(f)) {
		lines <- readLines(f, warn = FALSE)
		lines <- lines[trimws(lines) != "" & !startsWith(trimws(lines), "#")]
		return(lines)
	}
	return(c())
}

#' Check ignore patterns
#' @noRd
is_ignored <- function(file, patterns) {
	# 1. Always ignore these specific system files and standard temp files
	#    Allows implicit ignoring of Excel/Word lock files starting with ~$
	if (file %in% c(".formrignore", ".git", ".Rhistory") || startsWith(basename(file), "~$")) {
		return(TRUE)
	}
	
	for (p in patterns) {
		# 2. Handle Directory Ignores (e.g. "backup/")
		if (endsWith(p, "/")) {
			if (startsWith(file, p)) {
				return(TRUE)
			}
		}
		
		# 3. Handle File Ignores (e.g. ".DS_Store" or "*.png")
		# If the pattern has no slashes, we check if the filename matches (ignoring the folder path)
		if (!grepl("/", p)) {
			if (grepl(glob2rx(p), basename(file))) {
				return(TRUE)
			}
		}
		
		# 4. Handle Path Ignores (e.g. "files/secret.csv")
		# Check the full path against the pattern
		if (grepl(glob2rx(p), file)) {
			return(TRUE)
		}
	}
	
	return(FALSE)
}

#' Detect Changes
#' @noRd
detect_api_changes <- function(old_state, new_state) {
	old_files <- names(old_state)
	new_files <- names(new_state)
	
	added <- setdiff(new_files, old_files)
	deleted <- setdiff(old_files, new_files)
	
	common <- intersect(new_files, old_files)
	modified <- common[new_state[common] > old_state[common]]
	
	list(added = added, modified = modified, deleted = deleted)
}

#' Router: Handle changes
#' @noRd
handle_api_project_changes <- function(run_name, dir, changes) {
	to_process <- c(changes$added, changes$modified)
	
	# 1. SETTINGS & CSS/JS
	if (any(grepl("^(css/|js/|run_settings\\.json)", to_process))) {
		message(" Syncing Settings...")
		sync_run_settings(run_name, dir)
	}
	
	# 2. SURVEYS
	survey_files <- grep("^surveys/", to_process, value = TRUE)
	for (f in survey_files) {
		s_name <- tools::file_path_sans_ext(basename(f))
		
		# Check for spaces or other URL-unsafe characters
		if (grepl("[^a-zA-Z0-9_-]", s_name)) {
			suggested_name <- gsub("[^a-zA-Z0-9_-]", "_", s_name)
			
			message("[WARNING]  SKIPPED: '", basename(f), "'")
			message("   Reason: Survey names cannot contain spaces or special characters.")
			message("   Action: Please rename the file locally to '", suggested_name, ".xlsx'")
			next 
		}
		
		message("[INFO] Syncing Survey: ", s_name)
		
		tryCatch(
			{
				formr_api_upload_survey(file_path = file.path(dir, f))
				message("   [SUCCESS] Upload success")
			},
			error = function(e) {
				message("   [FAILED] Upload failed: ", e$message)
			}
		)
	}
	
	# 3. UPLOAD FILES
	asset_files <- grep("^files/", to_process, value = TRUE)
	if (length(asset_files) > 0) {
		message("Vm  Uploading ", length(asset_files), " file(s)...")
		for (f in asset_files) {
			try(formr_api_upload_file(run_name, file.path(dir, f)))
		}
	}
	
	# 4. DELETE FILES
	deleted_assets <- grep("^files/", changes$deleted, value = TRUE)
	if (length(deleted_assets) > 0) {
		message("Deleting ", length(deleted_assets), " file(s)...")
		
		for (del_f in deleted_assets) {
			raw_name <- basename(del_f)
			server_name <- gsub(" ", "_", raw_name)
			
			tryCatch(
				{
					formr_api_delete_file(run_name, server_name)
				},
				error = function(e) {
					message("   [WARNING] Could not delete '", server_name, "': ", e$message)
				}
			)
		}
	}
	
	# 5. STRUCTURE (Modified with Auto-Fix)
	if ("run_structure.json" %in% to_process) {
		message("[INFO]  Syncing Run Structure...")
		
		json_path <- file.path(dir, "run_structure.json")
		
		# The server crashes on empty objects "{}" for fields that expect strings/nulls.
		# This block reads the file, replaces ": {}" with ": null", and saves it back.
		tryCatch({
			txt <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
			
			# Regex to find "key": {} and replace with "key": null
			# Matches: quote, colon, optional space, open brace, close brace
			clean_txt <- gsub('":\\s*\\{\\}', '": null', txt)
			
			# Only write if changes were needed
			if (txt != clean_txt) {
				message("   [AUTO-FIX] Converting empty objects '{}' to 'null' for compatibility.")
				writeLines(clean_txt, json_path)
			}
		}, error = function(e) {
			warning("Failed to auto-clean JSON: ", e$message)
		})
		
		try(formr_api_run_structure(run_name, structure_json_path = json_path))
	}
}

#' Helper: Merge Settings + CSS + JS (with type safety)
#' @noRd
sync_run_settings <- function(run_name, dir) {
	settings_path <- file.path(dir, "run_settings.json")
	if (!file.exists(settings_path)) return()
	
	# Use jsonlite to read
	settings <- jsonlite::read_json(settings_path)
	
	# 1. Smart Asset Sync (Bi-directional)
	# If css/custom.css exists, read it. 
	# If css/ folder exists BUT file is gone, assume user deleted it -> clear settings.
	
	if (dir.exists(file.path(dir, "css"))) {
		css_file <- file.path(dir, "css", "custom.css")
		if (file.exists(css_file)) {
			settings$custom_css <- paste(readLines(css_file, warn=FALSE), collapse="\n")
		} else {
			settings$custom_css <- "" # Explicitly clear if file was deleted
		}
	}
	
	if (dir.exists(file.path(dir, "js"))) {
		js_file <- file.path(dir, "js", "custom.js")
		if (file.exists(js_file)) {
			settings$custom_js <- paste(readLines(js_file, warn=FALSE), collapse="\n")
		} else {
			settings$custom_js <- ""
		}
	}
	
	# 2. Recursive NULL Fix (Replaces the fragile Regex)
	# API requires explicit nulls, not empty lists list()
	fix_empty_lists <- function(x) {
		if (is.list(x) && length(x) == 0) return(NULL)
		if (is.list(x)) return(lapply(x, fix_empty_lists))
		return(x)
	}
	settings <- fix_empty_lists(settings)
	
	# 3. Upload
	tryCatch({
		formr_api_run_settings(run_name, settings)
		message("   [SETTINGS] Synced successfully.")
	}, error = function(e) warning("Failed to sync settings: ", e$message))
}

#' Sync Surveys from Structure to Local
#' Downloads Excel tables for all surveys defined in the structure.
#' @noRd
.sync_api_server_surveys <- function(struct, dir) {
	if (is.null(struct) || is.null(struct$units)) return()
	
	message("[INFO] Syncing survey tables...")
	
	# Ensure folder exists
	survey_dir <- file.path(dir, "surveys")
	if (!dir.exists(survey_dir)) dir.create(survey_dir, recursive = TRUE)
	
	count <- 0
	
	for (unit in struct$units) {
		if (identical(unit$type, "Survey")) {
			survey_name <- NULL
			if (is.list(unit$survey_data) && !is.null(unit$survey_data$name)) {
				survey_name <- unit$survey_data$name
			}

			# Only accept server-supplied survey names that match the pattern the
			# server itself uses for survey identifiers. This prevents a
			# malicious/compromised server from returning a name like
			# "../../.Rprofile" that would otherwise flow into file.path() below
			# and escape survey_dir when the XLSX is written to disk.
			if (!is.null(survey_name) &&
					!grepl("^[a-zA-Z][a-zA-Z0-9_]{2,64}$", survey_name)) {
				message("   [WARNING] Skipping survey with unsafe name: '", survey_name, "'")
				survey_name <- NULL
			}

			if (!is.null(survey_name)) {
				dest <- file.path(survey_dir, paste0(survey_name, ".xlsx"))
				tryCatch({
					path <- formr::formr_api_survey_structure(
						survey_name = survey_name, 
						format = "xlsx", 
						file_path = dest
					)
					if (!is.null(path)) count <- count + 1
				}, error = function(e) {
					message("   [WARNING] Failed to download '", survey_name, "': ", e$message)
				})
			}
		}
	}
	message(sprintf("[SUCCESS] Downloaded %d survey table(s).", count))
}

#' Sync Assets/Files from Server to Local
#' @noRd
.sync_api_server_files <- function(run_name, dir) {
	message("[INFO] Syncing assets/files...")
	
	# Ensure parent directory exists before proceeding
	if (!dir.exists(dir)) {
		warning("Skipping file sync: Parent directory '", dir, "' does not exist.")
		return()
	}
	
	tryCatch({
		files_list <- formr_api_request(endpoint = paste0("runs/", run_name, "/files"))
		
		if (length(files_list) > 0) {
			files_dir <- file.path(dir, "files")

			# Strict check for subfolder creation
			if (!dir.exists(files_dir)) {
				if (!dir.create(files_dir, recursive = TRUE)) {
					warning("   [WARNING] Could not create 'files' folder. Skipping downloads.")
					return()
				}
			}

			# Pin downloads to the authenticated origin. Accept exact match,
			# a super/sub relationship (e.g. api.formr.org ↔ formr.org), or
			# a shared parent so siblings like api.formr.org and cdn.formr.org
			# both resolve. The sibling check approximates eTLD+1 by
			# stripping the leading label; it is gated on each host having
			# ≥ 3 labels so that e.g. a.com and b.com don't collapse into
			# "com". This is imperfect on public-suffix TLDs like .co.uk
			# (example.co.uk and evil.co.uk would be treated as siblings) —
			# acceptable because this is defense-in-depth on top of the
			# basename() sanitisation of the destination path.
			session <- formr_api_session()
			expected_host <- if (!is.null(session)) tolower(session$base_url$hostname) else NULL
			expected_parts <- if (!is.null(expected_host)) strsplit(expected_host, ".", fixed = TRUE)[[1]] else NULL

			f_count <- 0
			for (f in files_list) {
				# Strip any directory component from the server-supplied name before
				# joining with files_dir — otherwise "../../.Rprofile" would escape
				# files_dir and overwrite arbitrary user-owned files.
				raw_name <- f$name
				base_name <- basename(raw_name)
				if (is.null(raw_name) || !nzchar(base_name) ||
						base_name %in% c(".", "..") ||
						grepl("[/\\\\]", raw_name) ||
						startsWith(base_name, ".")) {
					message("   [WARNING] Skipping file with unsafe name: '", raw_name, "'")
					next
				}
				safe_name <- gsub(" ", "_", base_name)
				dest <- file.path(files_dir, safe_name)

				parsed <- tryCatch(httr::parse_url(f$url), error = function(e) NULL)
				parsed_host <- if (!is.null(parsed$hostname)) tolower(parsed$hostname) else NULL
				host_ok <- FALSE
				if (!is.null(parsed_host) && !is.null(expected_host)) {
					parsed_parts <- strsplit(parsed_host, ".", fixed = TRUE)[[1]]
					host_ok <- identical(parsed_host, expected_host) ||
						endsWith(parsed_host, paste0(".", expected_host)) ||
						endsWith(expected_host, paste0(".", parsed_host)) ||
						(length(parsed_parts) >= 3 && length(expected_parts) >= 3 &&
						 identical(parsed_parts[-1], expected_parts[-1]))
				}
				if (is.null(parsed) || !isTRUE(parsed$scheme %in% c("http", "https")) || !host_ok) {
					message("   [WARNING] Skipping file with unsafe URL for '", raw_name, "'")
					next
				}

				tryCatch({
					download.file(f$url, dest, mode = "wb", quiet = TRUE)
					f_count <- f_count + 1
				}, error = function(e) message("   [WARNING] Failed to download file '", f$name, "'"))
			}
			message(sprintf("[SUCCESS] Downloaded %d file(s).", f_count))
		} else {
			message("   (No files found on server)")
		}
	}, error = function(e) warning("Failed to sync files: ", e$message))
}