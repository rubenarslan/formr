% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_to_formr.R
\name{formr_raw_results}
\alias{formr_raw_results}
\title{Download data from formr}
\usage{
formr_raw_results(survey_name, host = "https://formr.org")
}
\arguments{
\item{survey_name}{case-sensitive name of a survey your account owns}

\item{host}{defaults to https://formr.org}
}
\description{
After connecting to formr using \code{\link[=formr_connect]{formr_connect()}}
you can download data using this command.
}
\examples{
\dontrun{
formr_raw_results(survey_name = 'training_diary' )
}
}
