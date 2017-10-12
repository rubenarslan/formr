#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object. 
#'
#' @param results a formr results table with attributes set on items and scales
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#' 
#' @export
#' @examples
#' 
codebook = function(results, indent = '#') {
	stopifnot(exists("session", results))
	stopifnot(exists("created", results))
	stopifnot(exists("modified", results))
	stopifnot(exists("expired", results))
	stopifnot(exists("ended", results))
	old_opt = options('knitr.duplicate.label')$knitr.duplicate.label
	options(knitr.duplicate.label = 'allow')
	res = asis_knit_child(system.file("_codebook.Rmd", package = 'formr', mustWork = TRUE))
	options(knitr.duplicate.label = old_opt)
	res
}


#' codebook component for scales
#'
#'
#' @param scale a scale with attributes set
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#' 
#' @export
codebook_component_scale = function(scale, indent = '###') {
	stopifnot( exists("reliability", attributes(scale)))
	stopifnot( exists("likert_plot", attributes(scale)))
	stopifnot( exists("item", attributes(scale)))
	options = list(
		fig.path = paste0(knitr::opts_chunk$get("fig.path"), attributes(scale)$scale, "_"), 
		cache.path = paste0(knitr::opts_chunk$get("cache.path"), attributes(scale)$scale, "_")
	)
	asis_knit_child(system.file("_codebook_scale.Rmd", package = 'formr', mustWork = TRUE), options = options)
}

#' codebook component for single items
#'
#'
#' @param item an item with attributes set
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#' 
#' @export
codebook_component_single_item = function(item, indent = '###') {
	stopifnot( exists("item", attributes(item)))
	asis_knit_child(system.file("_codebook_item.Rmd", package = 'formr', mustWork = TRUE))
}

#' codebook component for scales
#'
#'
#' @param item an item without attributes set
#' @param item_name the item name
#' @param indent add # to this to make the headings in the components lower-level. defaults to beginning at h2
#' 
#' @export
codebook_component_fallback = function(item, item_name, indent = '###') {
	asis_knit_child(system.file("_codebook_fallback.Rmd", package = 'formr', mustWork = TRUE))
}


# todo:
# differentiate automagically between structural missings (didn't do this part because of showif), unfinished missings (didn't get this far), missings were people did not reply to optional items, and (later) missings where people said 'do not want to respond'
# visdat, vismiss
# skimr?
# scale/item sources, refs, with DOIs/links?
