#' custom markdown options for rmarkdown's pandoc
#'
#' @param add_to_format add these arguments to the default specification
#' @param fragment.only whether to get only a html fragment
#' @param ... all other arguments passed to \code{\link[rmarkdown:html_document]{html_document}}
#'
#' Custom rmarkdown input format options based on the standard \code{\link[rmarkdown:html_document]{html_document}}, but with options that you can specify. Find the format options here in the pandoc documentation: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
#' Pandoc's enhanced version of markdown includes syntax for footnotes, tables, flexible ordered lists, definition lists, fenced code blocks, superscript, subscript, strikeout, title blocks, automatic tables of contents, embedded LaTeX math, citations, and markdown inside HTML block elements or spoken in options: +escaped_line_breaks, +header_attributes, +yaml_metadata_block, +auto_identifiers, +implicit_header_references, +blank_before_blockquote, +fenced_code_blocks, +fenced_code_attributes, +line_blocks, +definition_lists, +startnum, +fancy_lists, +pipe_tables, +pandoc_title_block, +intraword_underscores, +strikeout, +superscript, +subscript, +tex_math_dollars, +raw_html, +markdown_in_html_blocks, +implicit_figures, +footnotes, +inline_notes, +citations.
#' The current default rmarkdown additions to Pandoc's enhanced markdown are: +autolink_bare_uris, +ascii_identifiers, +tex_math_single_backslash, -implicit_figures. 
#'
#' @export

markdown_custom_options = function (add_to_format = c('+autolink_bare_uris', '+ascii_identifiers', '+tex_math_single_backslash', '-implicit_figures'), fragment.only = FALSE, ...) 
{
	if(fragment.only) {
		output = rmarkdown::html_fragment(...)
	} else {
		output = rmarkdown::html_document(...)
	}
	
	if(stringr::str_sub(output$pandoc$from,1,8)=="markdown" ) {
		output$pandoc$from = paste0("markdown", paste( add_to_format, collapse=""))
	}
	output
}



#' github_markdown for rmarkdown
#'
#'
#' Custom template with github-flavoured markdown based on the standard \code{\link[rmarkdown:html_document]{html_document}}. Adds +pipe_tables, +raw_html, +tex_math_single_backslash, +fenced_code_blocks, +auto_identifiers, +ascii_identifiers, +backtick_code_blocks, +autolink_bare_uris, +intraword_underscores, +strikeout, +hard_line_breaks over markdown_strict. A number of pandoc features are disabled (see \code{\link{markdown_custom_options}}), but +yaml_metadata_block is re-enabled, so that it is possible to specify this output function using YAML.
#' 
#' @param fragment.only whether to get only a html fragment
#' @param ... all other arguments passed to \code{\link[rmarkdown:html_document]{html_document}}
#'
#' @export

markdown_github = function ( fragment.only = FALSE, ...) 
{
	if(fragment.only) {
		output = rmarkdown::html_fragment(...)
	} else {
		output = rmarkdown::html_document(...)
	}

	if(stringr::str_sub(output$pandoc$from,1,8)=="markdown" ) {
		output$pandoc$from = "markdown_github+yaml_metadata_block"
	}
	output
}


#' hard line breaks
#'
#'
#' Custom rmarkdown template based on the standard \code{\link[rmarkdown:html_document]{html_document}}, but with hard line breaks. Will add the pandoc "+hard_line_breaks" argument if the origin format is markdown.
#'
#' @param ... all other arguments passed to \code{\link[rmarkdown:html_document]{html_document}}
#' 
#' @export

markdown_hard_line_breaks = function (...) 
{
	markdown_custom_options(add_to_format = c('+autolink_bare_uris', '+ascii_identifiers', '+tex_math_single_backslash', '-implicit_figures', '+hard_line_breaks'), ...)
}

#' render text
#'
#'
#' Render text
#'
#' @param text that will be written to a tmp file and used as the input argument
#' @param ... all other arguments passed to \code{\link[rmarkdown:render]{render}}
#' 
#' @export

render_text = function (text, ...) 
{
	fileName = rmarkdown::render(input = write_to_file(text), ...)
	readChar(fileName, file.info(fileName)$size)
}

#' render text for formr
#'
#'
#' Render text
#'
#' @param text that will be written to a tmp file and used as the input argument
#' @param ... all other arguments passed to \code{\link[rmarkdown:render]{render}}
#' 
#' @export

formr_render = function (text, ...)
{
	fileName = rmarkdown::render(input = write_to_file(text), output_format = "formr::markdown_hard_line_breaks", output_options = list(fragment.only = TRUE), ...)
	readChar(fileName, file.info(fileName)$size)
}


write_to_file <- function(...){
	mytempfile <- tempfile();
	mytext <- eval(...)
	write(mytext, mytempfile);
	return(mytempfile)
}