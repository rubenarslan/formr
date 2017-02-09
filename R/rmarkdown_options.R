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

#' knit_child as is
#' 
#' This slightly modifies the [knitr::knit_child()] function to have different defaults.
#' - the environment defaults to the calling environment.
#' - the output receives the class knit_asis, so that the output will be rendered "as is" by knitr when calling inside a chunk (no need to set results='asis' as a chunk option).
#' - defaults to quiet = TRUE
#' 
#' Why default to the calling environment? Typically this function defaults to the global environment. This makes sense if you want to use knit_children in the same context as the rest of the document.
#' However, you may also want to use knit_children inside functions to e.g. summarise a regression using a set of commands (e.g. plot some diagnostic graphs and a summary for a regression nicely formatted).
#' 
#' Some caveats:
#' - the function has to return to the top-level. There's no way to [cat()] this from loops or an if-condition without without setting results='asis'. You can however concatenate these objects with [paste.knit_asis()]
#'
#' 
#' @param ... passed to [knitr::knit_child()]
#' @param quiet passed to [knitr::knit_child()]
#' @param options defaults to NULL.
#' @param envir passed to [knitr::knit_child()]
#'
#' @export
#' @examples
#' \dontrun{
#' # an example of a wrapper function that calls asis_knit_child with an argument
#' # ensures distinct paths for cache and figures, so that these calls can be looped in parallel
#' regression_summary = function(model) {
#'    child_hash = digest::digest(model)
#'    options = list(
#'        fig.path = paste0(knitr::opts_chunk$get("fig.path"), child_hash, "-"), 
#'        cache.path = paste0(knitr::opts_chunk$get("cache.path"), child_hash, "-"))
#'    asis_knit_child("_regression_summary.Rmd", options = options)
#' }
#' }
asis_knit_child = function(..., quiet = TRUE, options = NULL, envir = parent.frame()) {
	knitr::asis_output(knitr::knit_child(..., quiet = quiet, options = options, envir = envir))
}

#' paste.knit_asis
#' 
#' Helper function for knit_asis objects, useful when e.g. [asis_knit_child()] was used in a loop.
#'
#' Works like [paste()] with both the sep and the collapse argument set to two empty lines
#' 
#' @param ... passed to [paste()]
#' @param sep defaults to two empty lines, passed to [paste()]
#' @param collapse defaults to two empty lines, passed to [paste()]
#'
#' @export
#' @examples
#' paste.knit_asis("# Headline 1", "## Headline 2")
paste.knit_asis = function(..., sep = "\n\n\n", collapse = "\n\n\n") {
	knitr::asis_output(paste(..., sep = sep, collapse = collapse))
}


#' pander_handler
#' 
#' this is just a utility function that can be more safely assigned to render anything in knitr
#' using. It won't panderize already panderized objects.
#' opts_chunk$set(render = formr::pander_handler)
#' 
#'
#' @param x object to be printed
#' @param ... passed to pander
#' @param row.names row.names argument for pander, defaults to false here
#' @param dont_transform classes which aren't to be transformed, defaults to knit_asis
#' @export
#' @examples
#' data(ChickWeight)
#' pander_handler(xtabs(~ I(Time>10) + Diet, data = ChickWeight))
#' pander_handler(table(I(ChickWeight$Time>10)))
#' 

pander_handler = function(x, ..., row.names = FALSE, dont_transform = c("knit_asis")) {
	anyS3method = function(x) {
		classes = class(x)
		any(
			sapply(classes, FUN = function(classes) {
				!is.null(utils::getS3method('pander',classes, TRUE, environment(pander::pander)))
			})
		)
	}
	if (length(intersect(dont_transform, class(x))) == 0 && anyS3method(x)) {
		pander::pander(x, row.names = row.names, ...) # if pander has a method, we use it
	} else {
		res = withVisible(knitr::knit_print(x, ...))
		# indicate the htmlwidget result with a special class so we can attach
		# the figure caption to it later in wrap.knit_asis
		if (inherits(x, 'htmlwidget'))
			class(res$value) = c(class(res$value), 'knit_asis_htmlwidget')
		if (res$visible) res$value else invisible(res$value)
	}
}

#' build a bibliography bibtex file from your packrat lockfile
#' 
#' Packrat helps you maintain consistent package versions for a project. To be able to give due credit in a way that academics understand, it's helpful to be able to generate citations.
#' 
#'
#' @param overwrite_bib whether to overwrite an existing bibtex file of the same name
#' @param silent defaults to false. whether to cat out a nocite string to use in your header
#' @param cite_only_directly_called whether to call only the packages you called yourself (default) or also their dependencies
#' @param lockfile_path path to the packrat lock file to use
#' @param bibliography_path path to the bibtex file to generate
#' @param cite_R whether to cite R, defaults to true
#' @param cite_packrat whether to cite packrat even if it's not loaded explicitly, defaults to true
#' @export
#' 
packrat_bibliography = function(overwrite_bib = FALSE,
																silent = FALSE,
																cite_only_directly_called = TRUE,
																lockfile_path = "packrat/packrat.lock",
																bibliography_path = "packrat_bibliography.bibtex",
																cite_R = TRUE,
																cite_packrat = TRUE) {
	
	if(file.exists(bibliography_path) && ! overwrite_bib) {
		stop("Bibliography file existed and wasn't overwritten, specify overwrite_bib = TRUE.")
	}
	# use internal function to read lockfile (uses readDcf)
	stopifnot(file.exists(lockfile_path))
	lockfile = packrat:::readLockFile(lockfile_path)
	packages = packrat:::readLockFilePackages(lockfile_path)
	package_names = names(packages) # get pkg names
	
	citation_objects = list()
	if (cite_R) {
		citation_objects$R = utils::citation()
		citation_objects$R$note = paste("version", lockfile$r_version)
		citation_objects$R$key = "R"
	}
	
	for (i in seq_along(packages)) {
		pkg_name = packages[[i]]$name
		citation_obj = utils::citation(pkg_name)
		citation_obj$key = pkg_name	# by default the bibtex entries lack keys, we use the pkg name ,
		if (is.null(citation_obj$note) & !is.null(packages[[i]]$version)) {
			citation_obj$note = paste("version", packages[[i]]$version)
		}
		citation_objects[[ pkg_name ]] = citation_obj
	}
	
	bibliography = list()
	for (i in seq_along(citation_objects)) {
		name = names(citation_objects)[i]
		citation_obj = citation_objects[[i]]
		# don't lowercase R
		citation_obj[1]$title = stringr::str_replace_all(citation_obj[1]$title, "\\bR\\b", "{R}")
		# don't uppercase the package title
		citation_obj[1]$title = stringr::str_replace_all(citation_obj[1]$title, "^([a-zA-Z0-9.]+)+:", "{\\1}:")
		class(citation_obj) = c("citation", "bibentry")
		bibliography[[name]] = paste0(
			as.character(utils::toBibtex(citation_obj)),
			collapse = "\n")
	}
	
	bibliography = paste0(bibliography, collapse = "\n\n") # concatenate citations
	
	if (cite_only_directly_called) {
		package_names = packrat:::dirDependencies("./")
	}
	if (cite_packrat) {
		package_names = union("packrat", package_names)
	}
	
	# write bibliography to file
	writeLines(iconv(bibliography, to = "UTF-8"), bibliography_path, useBytes = TRUE)
	
	# generate YAML reference with nocite
	if (!silent) {
		cat(paste0("
							 bibliography: ", bibliography_path, "
							 nocite: |
							 ", paste0("@", c("R", package_names), collapse = " ")))
	}
	invisible(bibliography)
}
