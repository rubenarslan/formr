% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shorthands.R
\name{in_time_window}
\alias{in_time_window}
\title{checks whether the current time is in a certain time window}
\usage{
in_time_window(min, max)
}
\arguments{
\item{min}{POSIXct < max}

\item{max}{POSIXct > min}
}
\description{
supply min,max as POSIXct
}
\examples{
in_time_window(Sys.time() - 1, Sys.time() + 1)
}
