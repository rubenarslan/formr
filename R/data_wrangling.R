#' @importFrom dplyr "%>%"
#' @export
dplyr::`%>%`


#' Reverse labelled values
#' 
#' Taken from codebook package
#' reverse the underlying values for a numeric [haven::labelled()] vector while keeping the labels correct
#'
#' @param x a labelled vector
#' @return return the labelled vector with the underlying values having been reversed
#' @export
#' @examples
#' x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
#' x
#' reverse_labelled_values(x)
reverse_labelled_values <- function(x) {
	labels <- attributes(x)$labels
	values <- unname(labels)
	labels <- names(labels)
	if (is.factor(x) && is.null(labels) && !is.null(attributes(x)$levels)) {
		warning("Turning a factor into a labelled numeric vector")
		values <- seq_len(length(attributes(x)$levels))
		labels <- attributes(x)$levels
		x <- as.numeric(x)
	}
	missing_labels <- labels[is.na(values)]
	missing_values <- values[is.na(values)]
	labels <- labels[!is.na(values)]
	values <- values[!is.na(values)]
	if (
		length(values) == 0 ||
		(any(x > max(values) |
				 x < min(values), na.rm = TRUE))) {
		warning(deparse(substitute(x)), ": There are values outside the ",
						"labelled range. Reversion will only work if both the minimum ",
						"and maximum of the range are part of the responses.")
	}
	if (length(values) < length(unique(x)) ) {
		# if only some values have labels (e.g. extremes), make sure we include all
		possible_replies <- union(values, unique(x[!is.na(x)]))
	} else {
		possible_replies <- values
	}
	if (!is.numeric(possible_replies)) {
		warning(deparse(substitute(x)), " is not numeric and cannot be reversed.")
		x
	} else {
		range <- min(possible_replies):max(possible_replies)
		if (length(possible_replies) <
				length(range)) {
			possible_replies <- range
		}
		
		possible_replies <- sort(possible_replies)
		recode_replies <- stats::setNames(
			as.list(possible_replies), rev(possible_replies))
		new_x <- dplyr::recode(as.numeric(x), !!!recode_replies)
		
		attributes(new_x) <- attributes(x)
		attributes(new_x)$labels <- stats::setNames(
			c(rev(values), missing_values),
			c(labels, missing_labels))
		new_x
	}
}


as_same_type_as <- function(instance_of_target_class, object_to_convert) {
	target_class = class(instance_of_target_class)[1]
	if (target_class == 'numeric') {
		as.numeric(object_to_convert)
	} else {
		methods::as(object_to_convert, target_class)
	}
}

#' Aggregate variables and remember which variables this were
#'
#' Copied from codebook.
#' The resulting variables will have the attribute `scale_item_names` containing
#' the basis for aggregation. Its `label` attribute will refer to the common stem of the
#' aggregated variable names (if any), the number of variables, and the
#' aggregation function.
#'
#' @param items data.frame of the items that should be aggregated
#' @param fun aggregation function, defaults to rowMeans with na.rm = FALSE
#' @param stem common stem for the variables, specify if it should not be auto-detected
#' as the longest common stem of the variable names
#' @export
#' @examples
#' testdf <- data.frame(bfi_neuro_1 = rnorm(20), bfi_neuro_2 = rnorm(20),
#'                     bfi_neuro_3R = rnorm(20), age = rpois(20, 30))
#' item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
#' testdf$bfi_neuro <- aggregate_and_document_scale(testdf[, item_names])
#' testdf$bfi_neuro
aggregate_and_document_scale <- function(items, fun = rowMeans, stem = NULL) {
	new_scale <- fun(items)
	item_names <- names(items)
	attributes(new_scale)$scale_item_names <- item_names
	
	# find longest common stem
	if (is.null(stem)) {
		max_len <- min(nchar(item_names))
		for (l in max_len:0) {
			stem <- unique(stringr::str_sub(item_names, 1, l))
			if (length(stem) == 1) break
		}
	}
	# string trimming for idiots
	if (nchar(stem)) {
		stem <- stringr::str_match(stem, "^(.+?)_?$")[, 2]
	}
	
	attributes(new_scale)$label <- paste(ncol(items), stem, "items aggregated by",
																			 deparse(substitute(fun)))
	new_scale
}



#' Rescue lost attributes
#'
#' Taken from codebook
#' You can use this function if some of your items have lost their attributes during wrangling
#' Variables have to have the same name (Duh) and no attributes should be overwritten.
#' But use with care. Similar to `labelled::copy_labels()`.
#'
#'
#' @param df_no_attributes the data frame with missing attributes
#' @param df_with_attributes the data frame from which you want to restore attributes
#'
#' @export
#'
rescue_attributes <- function(df_no_attributes, df_with_attributes) {
	for (i in seq_along(names(df_no_attributes))) {
		var <- names(df_no_attributes)[i]
		if (var %in% names(df_with_attributes) &&
				is.null(attributes(df_no_attributes[[var]]))) {
			attributes(df_no_attributes[[var]]) <-
				attributes(df_with_attributes[[var]])
		} else {
			for (e in seq_along(names(attributes(df_with_attributes[[var]])))) {
				attrib_name <- names(attributes(df_with_attributes[[var]]))[e]
				if (!attrib_name %in% names(attributes(df_no_attributes[[var]]))) {
					attributes(df_no_attributes[[var]])[[attrib_name]] <-
						attributes(df_with_attributes[[var]])[[attrib_name]]
				}
			}
		}
	}
	df_no_attributes
}
