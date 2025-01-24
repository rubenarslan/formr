% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_to_formr.R
\name{formr_backup_surveys}
\alias{formr_backup_surveys}
\title{Backup surveys}
\usage{
formr_backup_surveys(
  survey_names,
  surveys = list(),
  save_path = "./",
  overwrite = FALSE,
  host = formr_last_host()
)
}
\arguments{
\item{survey_names}{case-sensitive names of surveys your account owns}

\item{surveys}{a list of survey data (from a run structure), optional}

\item{save_path}{path to save the study data, defaults to the study name}

\item{overwrite}{should existing files be overwritten?}

\item{host}{defaults to \code{\link[=formr_last_host]{formr_last_host()}}, which defaults to https://formr.org}
}
\description{
Backup surveys by downloading item lists, results, item displays and file lists.
}
\examples{
\dontrun{
formr_backup_surveys(survey_names = 'training_diary', save_path = 'surveys')
}
}
