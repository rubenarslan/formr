% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_to_formr.R
\name{formr_disconnect}
\alias{formr_disconnect}
\title{Disconnect from formr}
\usage{
formr_disconnect(host = formr_last_host())
}
\arguments{
\item{host}{defaults to \code{\link[=formr_last_host]{formr_last_host()}}, which defaults to https://formr.org}
}
\description{
Disconnects from formr if connected.
}
\examples{
\dontrun{
formr_disconnect()
}
}
