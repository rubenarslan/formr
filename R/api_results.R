#' Lower-level API Result Fetcher
#'
#' Fetches raw results. Advanced users can use this if they want 
#' completely raw data without any type coercion or processing.
#'
#' @param run_name Name of the run. Defaults to `.formr$run_name`,
#'   which is set automatically when the code runs inside an OpenCPU
#'   session on rforms.org.
#' @param surveys Optional character vector of survey names to filter by.
#' @param session_ids Optional character vector of session IDs to filter by.
#' @param item_names Optional character vector of item names to filter by.
#' @param join Logical. If TRUE, joins the results into a single data frame.
#' @export
formr_api_fetch_results <- function(run_name = .formr$run_name, surveys = NULL, session_ids = NULL, item_names = NULL, join = FALSE) {

	if (is.null(run_name) || !nzchar(run_name)) {
		stop("run_name is required (no .formr$run_name available -- pass it explicitly when running outside rforms.org)")
	}

	query <- list()
	if (!is.null(surveys)) query$surveys <- paste(surveys, collapse = ",")
	if (!is.null(session_ids)) query$sessions <- paste(session_ids, collapse = ",")
	if (!is.null(item_names)) query$items <- paste(item_names, collapse = ",")
	
	res <- formr_api_request(endpoint = paste0("runs/", run_name, "/results"), query = query)
	
	if (length(res) == 0) {
		message("[INFO] No results found.")
		return(dplyr::tibble(session = character()))
	}
	
	clean_json_to_df <- function(rows) {
		if (length(rows) == 0) return(dplyr::tibble(session = character()))
		df <- dplyr::bind_rows(rows)
		suppressMessages(readr::type_convert(df, col_types = readr::cols()))
	}
	
	shuffle_df <- NULL
	if ("shuffles" %in% names(res)) {
		raw_shuffles <- res$shuffles
		res$shuffles <- NULL
		if (length(raw_shuffles) > 0) {
			s_df <- clean_json_to_df(raw_shuffles)
			if (nrow(s_df) > 0 && "unit_id" %in% names(s_df)) {
				if (!"position" %in% names(s_df)) s_df$position <- NA
				shuffle_df <- s_df %>%
					dplyr::select(session = .data$run_session, .data$position, .data$unit_id, .data$group) %>%
					dplyr::mutate(position_col = dplyr::case_when(
						!is.na(.data$position) ~ paste0("shuffle_", .data$position),
						TRUE ~ paste0("shuffle_unit_", .data$unit_id)
					)) %>%
					dplyr::distinct(.data$session, .data$position_col, .keep_all = TRUE) %>%
					tidyr::pivot_wider(id_cols = "session", names_from = "position_col", values_from = "group", values_fn = list(group = toString))
			}
		}
	}
	
	results_list <- purrr::map(res, clean_json_to_df)
	if (!is.null(shuffle_df)) results_list$shuffles <- shuffle_df
	
	if (join) return(.join_results(results_list))
	
	if (length(results_list) == 1 && is.null(shuffle_df)) return(results_list[[1]])
	return(results_list)
}

#' Get and Process Run Results
#'
#' This is the main function for scientists. It fetches data from the API,
#' automatically cleans types (dates/numbers), reverses items, computes scales,
#' and joins everything into one dataframe.
#'
#' @param run_name Name of the run. Defaults to `.formr$run_name`, which
#'   is set automatically when the code runs inside an OpenCPU session on
#'   rforms.org -- so portable run code can omit this argument.
#' @param ... Filters passed to API (e.g. `surveys = c("Daily", "Intake")`, `session_ids = "..."`).
#' @param compute_scales Logical. Should scales (e.g. `extraversion`) be computed from items (e.g. `extra_1`, `extra_2`)?
#' @param join Logical. If TRUE (default), joins all surveys into one wide dataframe.
#' @param remove_test_sessions Logical. Filter out sessions marked as testing?
#' @param verbose Logical. Print progress messages?
#'
#' @return A processed tibble with class `formr_results`.
#' @export
formr_api_results <- function(run_name = .formr$run_name,
															...,
															compute_scales = TRUE,
															join = TRUE,
															remove_test_sessions = TRUE,
															verbose = TRUE) {

	if (is.null(run_name) || !nzchar(run_name)) {
		stop("run_name is required (no .formr$run_name available -- pass it explicitly when running outside rforms.org)")
	}

	log_msg <- function(...) if(verbose) message(sprintf(...))
	
	# 1. Fetch Metadata
	log_msg("Fetching metadata for '%s'...", run_name)
	structure <- formr_api_run_structure(run_name)
	
	# 2. Prepare Test Session Filter
	test_session_codes <- character(0)
	
	if (remove_test_sessions) {
		tryCatch({
			all_sessions <- formr_api_sessions(run_name, limit = 100000)
			if ("testing" %in% names(all_sessions) && "session" %in% names(all_sessions)) {
				test_sessions_df <- all_sessions[all_sessions$testing == 1 | all_sessions$testing == TRUE, ]
				test_session_codes <- unique(test_sessions_df$session)
				if (length(test_session_codes) > 0) {
					log_msg("... Identified %d test sessions via API.", length(test_session_codes))
				}
			}
		}, error = function(e) {
			warning("Could not fetch session metadata from API. Falling back to 'XXX' pattern matching.")
		})
	}
	
	# 3. Fetch Results
	log_msg("Fetching results...")
	raw_results_list <- formr_api_fetch_results(run_name, ..., join = FALSE)
	
	# --- SAFE FINGERPRINTING (Single DF Case) ---
	if (inherits(raw_results_list, "data.frame")) {
		data_cols <- names(raw_results_list)
		possible_matches <- c()
		
		if (!is.null(structure$units)) {
			for (unit in structure$units) {
				if (unit$type == "Survey" && !is.null(unit$survey_data$items)) {
					meta_items <- dplyr::bind_rows(unit$survey_data$items)
					relevant_items <- meta_items$name[ !meta_items$type %in% c("note", "submit", "block") ]
					
					overlap_count <- sum(relevant_items %in% data_cols)
					total_relevant <- length(relevant_items)
					
					if (total_relevant > 0 && (overlap_count / total_relevant) > 0.5) {
						possible_matches <- c(possible_matches, unit$survey_data$name)
					}
				}
			}
		}
		
		if (length(possible_matches) == 1) {
			guessed_name <- possible_matches[1]
			log_msg("... Auto-detected survey: '%s'", guessed_name)
			raw_results_list <- list(raw_results_list)
			names(raw_results_list) <- guessed_name
		} else {
			warning("Could not match single result dataframe to Run Structure. Returning raw data.")
			return(raw_results_list)
		}
	}
	
	# 4. Processing Pipeline
	process_single_survey <- function(df, survey_name) {
		if (nrow(df) == 0) return(df)
		
		# 1. Filter Test Sessions
		if (remove_test_sessions && "session" %in% names(df)) {
			initial_count <- nrow(df)
			if (length(test_session_codes) > 0) df <- df[ !df$session %in% test_session_codes, ]
			df <- df[ !grepl("XXX", df$session), ]
			
			n_removed <- initial_count - nrow(df)
			if (n_removed > 0) {
				df <- .log_action(df, "Filter", sprintf("Removed %d test sessions", n_removed))
			}
		}
		
		# 2. Identify and Drop Non-Data Columns
		survey_meta <- .extract_items_for_survey(structure, survey_name)
		if (is.null(survey_meta)) return(df)
		
		# Define types that DO NOT contain user data
		ignored_types <- c("note", "submit", "block", "mcq_header", "button", "image")
		
		# Get names of items that are NOT in the ignored list
		data_item_names <- survey_meta$name[ !survey_meta$type %in% ignored_types ]
		
		# Always keep these system columns
		system_cols <- c("session", "created", "modified", "ended", "expired")
		
		# Perform the subset
		final_cols <- intersect(names(df), c(system_cols, data_item_names))
		
		dropped_count <- length(setdiff(names(df), final_cols))
		df <- df[, final_cols, drop = FALSE]
		
		if (dropped_count > 0) {
			df <- .log_action(df, "Clean", sprintf("Dropped %d decorative columns (notes/pics/headers)", dropped_count))
		}
		
		# 3. Standard Processing (Recognise, Reverse, Aggregate)
		df <- formr_api_recognise(survey_meta, df) 
		df <- formr_api_reverse(df, survey_meta)   
		
		if (compute_scales) {
			df <- formr_api_aggregate(df, survey_meta)
		}
		return(df)
	}
	
	# 5. Apply & Join
	log_msg("Processing data...")
	processed_list <- purrr::imap(raw_results_list, function(df, name) {
		if (name == "shuffles") return(df)
		process_single_survey(df, name)
	})
	
	if (join) {
		log_msg("Joining surveys...")
		result <- .join_results(processed_list)
		
		survey_names <- names(processed_list)
		survey_names <- survey_names[survey_names != "shuffles"]
		if(length(survey_names) > 1) {
			result <- .log_action(result, "Join", sprintf("Joined %d surveys: %s", length(survey_names), paste(survey_names, collapse=", ")))
		}
		
		class(result) <- c("formr_results", class(result))
		return(result)
	}
	
	return(processed_list)
}

#' Apply Type Definitions and Labels
#' @param item_list A data frame containing item metadata.
#' @param results A data frame containing the raw results.
#' @export
formr_api_recognise <- function(item_list, results) {
	if (is.null(results) || nrow(results) == 0) return(results)
	if (is.null(item_list) || nrow(item_list) == 0) return(results)
	
	# 1. Timestamps
	time_cols <- intersect(names(results), c("created", "modified", "ended"))
	for(col in time_cols) {
		if(!inherits(results[[col]], "POSIXct")) {
			results[[col]] <- tryCatch(as.POSIXct(results[[col]]), error = function(e) results[[col]])
		}
		attr(results[[col]], "label") <- paste("Timestamp:", tools::toTitleCase(col))
	}
	
	# 2. Items
	items_to_process <- item_list[item_list$name %in% names(results), ]
	if(nrow(items_to_process) == 0) return(results)
	
	for (i in seq_len(nrow(items_to_process))) {
		item_row <- items_to_process[i, ]
		name <- item_row$name
		type <- item_row$type
		
		# Extract Label (but don't apply it yet)
		raw_label <- if(!is.null(item_row$label)) item_row$label else ""
		clean_label <- trimws(gsub("<[^>]+>", "", raw_label))
		
		# Choice Handling (Labelled/Factor)
		if ("choices" %in% names(item_row) && !is.null(item_row$choices[[1]])) {
			choices <- item_row$choices[[1]]
			
			# Prune NULLs
			if (is.list(choices)) {
				choices <- choices[ !vapply(choices, is.null, logical(1)) ]
			}
			
			if (length(choices) > 0) {
				ch_names  <- names(choices)
				ch_values <- vapply(choices, function(x) {
					if (is.null(x) || length(x) == 0) return(NA_character_)
					as.character(x[[1]])
				}, character(1))
				
				if (length(ch_names) != length(ch_values)) next # Skip silently
				
				# Detect if choices are Value = Label or Label = Value
				names_num  <- suppressWarnings(as.numeric(ch_names))
				values_num <- suppressWarnings(as.numeric(ch_values))
				
				if (!any(is.na(names_num))) {
					data_values <- names_num
					value_labels <- as.character(ch_values)
				} else if (!any(is.na(values_num))) {
					data_values <- values_num
					value_labels <- ch_names
				} else {
					data_values <- as.character(ch_values)
					value_labels <- ch_names
				}
				
				# Match the type of data_values to the type of the actual column
				if (is.numeric(results[[name]])) {
					data_values <- suppressWarnings(as.numeric(data_values))
				} else {
					data_values <- as.character(data_values)
				}
				
				# Apply haven labels silently
				tryCatch({
					results[[name]] <- haven::labelled(results[[name]], stats::setNames(data_values, value_labels))
				}, error = function(e) NULL)
			}
		} 
		# Explicit Types for non-choice items
		else if (type %in% c("date", "datetime")) {
			results[[name]] <- tryCatch(as.POSIXct(results[[name]]), error = function(e) results[[name]])
		} else if (type %in% c("number", "range", "calculate")) {
			results[[name]] <- suppressWarnings(as.numeric(results[[name]]))
		}
		
		# APPLY LABEL TO FINAL VECTOR
		# Placed here so it isn't stripped by the conversions above
		if (nchar(clean_label) > 0) {
			attr(results[[name]], "label") <- clean_label
		}
	}
	
	if (exists(".log_action", mode="function")) {
		results <- .log_action(results, "Recognize", sprintf("Applied types/labels to %d items", nrow(items_to_process)))
	}
	results
}

#' Reverse Items and Update Labels
#' 
#' Reverses numeric items ending in 'R' based on metadata bounds.
#' Critically, it also updates `haven::labelled` attributes so that 
#' the text labels point to the new, reversed values.
#' 
#' @param results A data frame containing the results.
#' @param item_list A data frame containing item metadata.
#' @export
formr_api_reverse <- function(results, item_list) {
	if (is.null(item_list)) return(results)
	
	item_names <- names(results)
	reversed_vars <- item_names[stringr::str_detect(item_names, "(?i)[a-z0-9_]+?[0-9]+R$")]
	processed_count <- 0
	
	for (var in reversed_vars) {
		val_vec <- results[[var]]
		meta <- item_list[item_list$name == var, ]
		if(nrow(meta) == 0) next
		
		choices <- if(is.list(meta$choices)) meta$choices[[1]] else meta$choices
		if(is.null(choices)) next 
		
		# Robust Bounds Detection
		meta_vals <- suppressWarnings(as.numeric(unlist(choices)))
		if(all(is.na(meta_vals)) && !is.null(names(choices))) {
			meta_vals <- suppressWarnings(as.numeric(names(choices)))
		}
		
		if(all(is.na(meta_vals))) {
			warning(sprintf("Skipped reversal for '%s': Non-numeric choices.", var))
			next 
		}
		
		reversal_const <- max(meta_vals, na.rm=TRUE) + min(meta_vals, na.rm=TRUE)
		
		# Safe Data Conversion
		raw_data <- as.vector(val_vec)
		if (is.character(raw_data)) {
			converted <- suppressWarnings(as.numeric(raw_data))
			if (sum(is.na(converted)) > sum(is.na(raw_data))) {
				warning(sprintf("Skipped reversal for '%s': Data contains non-numeric strings.", var))
				next
			}
			raw_data <- converted
		}
		
		reversed_numeric <- reversal_const - raw_data
		
		# Handle Attributes
		if (inherits(val_vec, "haven_labelled")) {
			old_labels <- attr(val_vec, "labels")
			if (!is.null(old_labels)) {
				old_lbl_vals <- suppressWarnings(as.numeric(unname(old_labels)))
				
				if (any(is.na(old_lbl_vals))) {
					warning(sprintf("Reversed '%s' but dropped labels (labels were non-numeric).", var))
					results[[var]] <- reversed_numeric
				} else {
					new_vals <- reversal_const - old_lbl_vals
					results[[var]] <- haven::labelled(reversed_numeric, sort(stats::setNames(new_vals, names(old_labels))))
				}
			} else {
				results[[var]] <- reversed_numeric
			}
			if (!is.null(attr(val_vec, "label"))) attr(results[[var]], "label") <- attr(val_vec, "label")
		} else {
			results[[var]] <- reversed_numeric
			if (!is.null(attr(val_vec, "label"))) attr(results[[var]], "label") <- attr(val_vec, "label")
		}
		
		attr(results[[var]], "reversed") <- TRUE
		processed_count <- processed_count + 1
	}
	
	if (processed_count > 0) {
		results <- .log_action(results, "Reverse", sprintf("Reversed %d items (e.g. %s)", processed_count, reversed_vars[1]))
	}
	results
}

#' Aggregate Scales
#' 
#' @param results A data frame/tibble containing the run results.
#' @param item_list A data frame containing item metadata (names, types, choices).
#' @param min_items Minimum number of valid items required to calculate a mean (default 2).
#' @importFrom stats var
#' @export
formr_api_aggregate <- function(results, item_list, min_items = 2) {
	item_names <- names(results)
	stems <- stringr::str_match(item_names, "^([a-zA-Z0-9_]+?)[_]?\\d+[R]?$")[, 2]
	unique_stems <- unique(stats::na.omit(stems))
	unique_stems <- unique_stems[!grepl("^shuffle", unique_stems)]
	
	scales_created <- 0
	
	for (scale in unique_stems) {
		if (scale %in% names(results)) next 
		
		pattern <- paste0("^", scale, "[_]?\\d+[R]?$")
		scale_cols <- item_names[stringr::str_detect(item_names, pattern)]
		
		# Filter for numeric columns only
		is_num <- vapply(results[scale_cols], function(x) is.numeric(x) || inherits(x, "haven_labelled"), logical(1))
		valid_cols <- scale_cols[is_num]
		
		if (length(valid_cols) >= min_items) {
			subset_df <- as.data.frame(lapply(results[valid_cols], as.numeric))
			
			# 1. Mean
			results[[scale]] <- rowMeans(subset_df, na.rm = FALSE)
			
			# 2. Reliability (Cronbach's Alpha)
			alpha_val <- NA
			if(ncol(subset_df) > 1) {
				k <- ncol(subset_df)
				var_sum <- sum(apply(subset_df, 2, var, na.rm = TRUE))
				var_tot <- var(rowSums(subset_df, na.rm = TRUE), na.rm = TRUE)
				alpha_val <- (k / (k - 1)) * (1 - (var_sum / var_tot))
			}
			
			# 3. Attributes
			attr(results[[scale]], "scale_items") <- valid_cols
			attr(results[[scale]], "reliability_alpha") <- alpha_val
			attr(results[[scale]], "label") <- sprintf("%s Scale (Mean)", scale)
			
			scales_created <- scales_created + 1
		}
	}
	
	if (scales_created > 0) {
		results <- .log_action(results, "Scale", sprintf("Computed %d scales (means)", scales_created))
	}
	results
}

#' Helper: Join results safely
#' @noRd
.join_results <- function(results_list) {
	if (length(results_list) == 0) return(dplyr::tibble(session = character()))
	if (length(results_list) == 1) return(results_list[[1]])
	
	results_list_renamed <- purrr::imap(results_list, function(df, name) {
		if (name == "shuffles") return(df)
		
		cols_to_rename <- setdiff(names(df), "session")
		if (length(cols_to_rename) > 0) {
			new_names <- paste0(cols_to_rename, "_", name)
			names(df)[match(cols_to_rename, names(df))] <- new_names
		}
		return(df)
	})
	
	purrr::reduce(results_list_renamed, function(x, y) dplyr::full_join(x, y, by = "session"))
}

#' Helper: Extract items for survey
#' @noRd
.extract_items_for_survey <- function(structure, survey_name) {
	if (is.null(structure$units)) return(NULL)
	
	for (unit in structure$units) {
		if (unit$type == "Survey" && !is.null(unit$survey_data$name)) {
			if (trimws(unit$survey_data$name) == trimws(survey_name)) {
				items <- unit$survey_data$items
				if (is.null(items) || length(items) == 0) return(NULL)
				
				df <- dplyr::bind_rows(items)
				
				# If an item name appears multiple times, aggregate its choices
				if ("name" %in% names(df) && any(duplicated(df$name))) {
					df <- df %>%
						dplyr::group_by(.data$name) %>%
						dplyr::mutate(
							# Combine all fragmented choices into a single list
							choices = if ("choices" %in% names(.)) list(do.call(c, .data$choices)) else list(NULL)
						) %>%
						dplyr::slice(1) %>% # Keep only the first row for the core metadata
						dplyr::ungroup()
				}
				
				return(df)
			}
		}
	}
	return(NULL)
}

#' Processing Log Helper
#' @noRd
.log_action <- function(df, category, message) {
	history <- attr(df, "processing_history")
	if (is.null(history)) history <- list()
	
	entry <- list(
		timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
		category = category,
		message = message
	)
	history[[length(history) + 1]] <- entry
	
	attr(df, "processing_history") <- history
	df
}

#' Summarize Processing History
#' 
#' Prints a human-readable audit trail of all data cleaning steps.
#' 
#' @param object A `formr_results` object.
#' @param ... Additional arguments passed to summary (ignored).
#' @export
summary.formr_results <- function(object, ...) {
	history <- attr(object, "processing_history")
	
	if (is.null(history) || length(history) == 0) {
		cat("No processing history recorded.\n")
		return(invisible(NULL))
	}
	
	cat("=== Processing Audit Trail ===\n\n")
	categories <- unique(vapply(history, function(x) x$category, character(1)))
	
	for (cat in categories) {
		cat(toupper(cat), "\n")
		cat(paste0(rep("-", nchar(cat)), collapse = ""), "\n")
		
		cat_entries <- Filter(function(x) x$category == cat, history)
		for (entry in cat_entries) {
			cat("  - ", entry$message, "\n", sep = "")
		}
		cat("\n")
	}
	invisible(NULL)
}
