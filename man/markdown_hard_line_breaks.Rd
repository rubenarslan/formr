% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown_options.R
\name{markdown_hard_line_breaks}
\alias{markdown_hard_line_breaks}
\title{hard line breaks}
\usage{
markdown_hard_line_breaks(...)
}
\arguments{
\item{...}{all other arguments passed to \code{\link[rmarkdown:html_document]{rmarkdown::html_document()}}}
}
\description{
Custom rmarkdown template based on the standard \code{\link[rmarkdown:html_document]{rmarkdown::html_document()}}, but with hard line breaks. Will add the pandoc '+hard_line_breaks' argument if the origin format is markdown.
}
