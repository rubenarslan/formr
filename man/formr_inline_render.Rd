% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown_options.R
\name{formr_inline_render}
\alias{formr_inline_render}
\title{render inline text for formr}
\usage{
formr_inline_render(text, self_contained = TRUE, ...)
}
\arguments{
\item{text}{that will be written to a tmp file and used as the input argument}

\item{self_contained}{passed to \link{markdown_custom_options}}

\item{...}{all other arguments passed to \code{\link[rmarkdown:render]{rmarkdown::render()}}}
}
\description{
Render text
}
