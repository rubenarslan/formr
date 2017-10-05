#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object. 
#'
#' @param results a formr results table with attributes set on items and scales
#' 
#' @export
#' @examples
#' 
codebook = function(results, indent = '#') {
	asis_knit_child(system.file("_codebook.Rmd", package = 'formr', mustWork = TRUE))
}


#' @export
codebook_component_scale = function(scale, indent = '###') {
	stopifnot( exists("reliability", attributes(scale)))
	asis_knit_child(system.file("_codebook_scale.Rmd", package = 'formr', mustWork = TRUE))
}

#' @export
codebook_component_single_item = function(item, indent = '###') {
	stopifnot( exists("item", attributes(item)))
	asis_knit_child(system.file("_codebook_item.Rmd", package = 'formr', mustWork = TRUE))
}

#' @export
codebook_component_fallback = function(item, item_name, indent = '###') {
	asis_knit_child(system.file("_codebook_fallback.Rmd", package = 'formr', mustWork = TRUE))
}
