.datatable.aware = TRUE

#' custom markdown options for rmarkdown's pandoc
#'
#' @param add_to_format add these arguments to the default specification
#' @param fragment.only whether to get only a html fragment
#' @param section_divs whether to disable --section-divs (headings generate section including everything up to the next same-or-higher-level heading)
#' @param break_on_error should an error in the R code execution interrupt the rendering or should rendering continue, defaults to FALSE
#' @param ... all other arguments passed to [rmarkdown::html_document()]
#'
#' Custom rmarkdown input format options based on the standard [rmarkdown::html_document()], but with options that you can specify. Find the format options here in the pandoc documentation: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
#' Pandoc's enhanced version of markdown includes syntax for footnotes, tables, flexible ordered lists, definition lists, fenced code blocks, superscript, subscript, strikeout, title blocks, automatic tables of contents, embedded LaTeX math, citations, and markdown inside HTML block elements or spoken in options: +escaped_line_breaks, +header_attributes, +yaml_metadata_block, +auto_identifiers, +implicit_header_references, +blank_before_blockquote, +fenced_code_blocks, +fenced_code_attributes, +line_blocks, +definition_lists, +startnum, +fancy_lists, +pipe_tables, +pandoc_title_block, +intraword_underscores, +strikeout, +superscript, +subscript, +tex_math_dollars, +raw_html, +markdown_in_html_blocks, +implicit_figures, +footnotes, +inline_notes, +citations.
#' The current default rmarkdown additions to Pandoc's enhanced markdown are: +autolink_bare_uris, +ascii_identifiers, +tex_math_single_backslash, -implicit_figures. 
#'
#' @export

markdown_custom_options = function(add_to_format = c("+autolink_bare_uris", 
  "+ascii_identifiers", "+tex_math_single_backslash", "-implicit_figures"), 
  fragment.only = FALSE, section_divs = TRUE, break_on_error = FALSE, 
  ...) {
  if (fragment.only) {
    output = rmarkdown::html_fragment(...)
  } else {
    output = rmarkdown::html_document(...)
  }
  
  # always show errors inline, try to finish
  output$knitr$opts_chunk$error = !break_on_error
  
  if (stringr::str_sub(output$pandoc$from, 1, 8) == "markdown") {
    output$pandoc$from = paste0("markdown", paste(add_to_format, 
      collapse = ""))
  }
  if (!section_divs) {
    output$pandoc$to = "html5"
    output$pandoc$args = setdiff(output$pandoc$args, "--section-divs")
  }
  
  output
}



#' github_markdown for rmarkdown
#'
#'
#' Custom template with github-flavoured markdown based on the standard [rmarkdown::html_document()]. Adds +pipe_tables, +raw_html, +tex_math_single_backslash, +fenced_code_blocks, +auto_identifiers, +ascii_identifiers, +backtick_code_blocks, +autolink_bare_uris, +intraword_underscores, +strikeout, +hard_line_breaks over markdown_strict. A number of pandoc features are disabled (see [markdown_custom_options()]), but +yaml_metadata_block is re-enabled, so that it is possible to specify this output function using YAML.
#' 
#' @param fragment.only whether to get only a html fragment
#' @param break_on_error should an error in the R code execution interrupt the rendering or should rendering continue, defaults to FALSE
#' @param ... all other arguments passed to [rmarkdown::html_document()]
#'
#' @export

markdown_github = function(fragment.only = FALSE, break_on_error = FALSE, 
  ...) {
  if (fragment.only) {
    output = rmarkdown::html_fragment(...)
  } else {
    output = rmarkdown::html_document(...)
  }
  
  output$knitr$opts_chunk$error = !break_on_error
  
  if (stringr::str_sub(output$pandoc$from, 1, 8) == "markdown") {
    output$pandoc$from = "markdown_github+yaml_metadata_block"
  }
  output
}


#' hard line breaks
#'
#'
#' Custom rmarkdown template based on the standard [rmarkdown::html_document()], but with hard line breaks. Will add the pandoc '+hard_line_breaks' argument if the origin format is markdown.
#'
#' @param ... all other arguments passed to [rmarkdown::html_document()]
#' 
#' @export

markdown_hard_line_breaks = function(...) {
  markdown_custom_options(add_to_format = c("+autolink_bare_uris", 
    "+ascii_identifiers", "+tex_math_single_backslash", "-implicit_figures", 
    "+hard_line_breaks"), ...)
}

#' render text
#'
#'
#' Render text
#'
#' @param text that will be written to a tmp file and used as the input argument
#' @param ... all other arguments passed to [rmarkdown::render()]
#' 
#' @export

render_text = function(text, ...) {
  fileName = rmarkdown::render(input = write_to_file(text, 
    ext = ".Rmd"), ...)
  readChar(fileName, file.info(fileName)$size)
}


#' render inline text for formr
#'
#'
#' Render text
#'
#' @param text that will be written to a tmp file and used as the input argument
#' @param self_contained passed to \link{markdown_custom_options}
#' @param ... all other arguments passed to [rmarkdown::render()]
#' 
#' @export

formr_inline_render = function(text, self_contained = TRUE, ...) {
  fileName = rmarkdown::render(input = write_to_file(text, 
    name = "knit", ext = ".Rmd"), output_format = formr::markdown_hard_line_breaks(self_contained = self_contained, 
    fragment.only = TRUE, section_divs = FALSE), ...)
  readChar(fileName, file.info(fileName)$size)
}

#' knit rmarkdown to markdown for formr
#'
#'
#' Render text
#'
#' @param text rmarkdown that will be knit
#' 
#' @export

formr_knit = function(text) {
   knitr::knit(text = text, quiet = TRUE, encoding = "utf-8")
}

#' render inline text for formr
#'
#'
#' Render text
#'
#' @param text that will be passed to knitr
#' 
#' @export

formr_render_commonmark = function(text) {
	commonmark::markdown_html(text = knitr::knit(text = text, 
																							 quiet = TRUE, encoding = "utf-8"), hardbreaks = TRUE, 
														smart = TRUE)
}

#' render text for formr
#'
#'
#' Render text
#'
#' @param text that will be written to a tmp file and used as the input argument
#' @param self_contained passed to \link{markdown_custom_options}
#' @param ... all other arguments passed to [rmarkdown::render()]
#' 
#' @export

formr_render = function(text, self_contained = FALSE, ...) {
  fileName = rmarkdown::render(input = write_to_file(text, 
    name = "knit", ext = ".Rmd"), output_format = formr::markdown_hard_line_breaks(self_contained = self_contained, 
    fragment.only = FALSE), clean = T, quiet = T, ...,)
  fileName
}



write_to_file <- function(..., name = NULL, ext = ".Rmd") {
  if (is.null(name)) {
    filename <- paste0(tempfile(), ext)
  } else {
    filename = paste0(name, ext)
  }
  mytext <- eval(...)
  write(mytext, filename)
  return(filename)
}

#' knit prefixed
#'
#'
#' Knit using knitr, but prefix file name to figure and cache folder (to knit in parallel on e.g. a cluster)
#'
#' @param input input document
#' @param ... all arguments passed to [knitr::knit()]
#' 
#' @export
#' @import knitr
#' 
knit_prefixed = function(input, ...) {
  prefix = tools::file_path_sans_ext(basename(input))
  opts_chunk$set(fig.path = paste0(prefix, "/figure/"), cache.path = paste0(prefix, 
    "/cache/"))
  knit(input, ...)
}

#' word_document from rmarkdown, but has an added option not to break on error
#'
#'
#' Exactly like [rmarkdown::word_document()], but with one added argument
#' 
#' @param ... all other arguments passed to [rmarkdown::word_document()]
#' @param break_on_error should an error in the R code execution interrupt the rendering or should rendering continue, defaults to FALSE
#'
#' @export

word_document = function(..., break_on_error = FALSE) {
  output = rmarkdown::word_document(...)
  output$knitr$opts_chunk$error = !break_on_error
  output
}
