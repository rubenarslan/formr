#' Generate rmarkdown codebook
#'
#' If you pass the object resulting from a call to formr_results to this function, it will generate a markdown codebook for this object. 
#'
#' @param results a formr results table with attributes set on items and scales
#' 
#' @export
#' @examples
#' 
codebook = function(results) {
	codebook_component_scale(results$bfi_extra)
}


codebook_component_scale = function(scale) {
	stopifnot( exists("reliability", attributes(scale)))
	indent = '###'
	asis_knit_child(system.file("_codebook_scale.Rmd", package = 'formr', mustWork = TRUE))
}
