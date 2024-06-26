% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_to_formr.R
\name{formr_aggregate}
\alias{formr_aggregate}
\title{Aggregate data based on item table}
\usage{
formr_aggregate(
  survey_name,
  item_list = formr_items(survey_name, host = host),
  results = formr_raw_results(survey_name, host = host),
  host = formr_last_host(),
  compute_alphas = FALSE,
  fallback_max = 5,
  plot_likert = FALSE,
  quiet = FALSE,
  aggregation_function = rowMeans,
  ...
)
}
\arguments{
\item{survey_name}{case-sensitive name of a survey your account owns}

\item{item_list}{an item_list, will be auto-retrieved based on survey_name if omitted}

\item{results}{survey results, will be auto-retrieved based on survey_name if omitted}

\item{host}{defaults to \code{\link[=formr_last_host]{formr_last_host()}}, which defaults to https://formr.org}

\item{compute_alphas}{deprecated, functionality migrated to codebook package}

\item{fallback_max}{defaults to 5 - if the item_list is set to null, we will use this to reverse}

\item{plot_likert}{deprecated, functionality migrated to codebook package}

\item{quiet}{defaults to FALSE - If set to true, likert plots and reliability computations are not echoed.}

\item{aggregation_function}{defaults to rowMeans with na.rm = FALSE}

\item{...}{passed to  \code{\link[psych:alpha]{psych::alpha()}}}
}
\description{
If you've retrieved an item table using \code{\link[=formr_items]{formr_items()}} you can use this
function to aggregate your multiple choice items into mean scores.
If you do not have a item table (e.g. your data was not collected using formr, you don't want another HTTP request in a time-sensitive process).
Example: If your data contains Extraversion_1, Extraversion_2R and Extraversion_3, there will be two new variables in the result: Extraversion_2 (reversed to align with _1 and _2) and Extraversion, the mean score of the three.
}
\examples{
results = jsonlite::fromJSON(txt = 
	system.file('extdata/gods_example_results.json', package = 'formr', mustWork = TRUE))
items = formr_items(path = 
	system.file('extdata/gods_example_items.json', package = 'formr', mustWork = TRUE))
results = formr_recognise(item_list = items, results = results)
agg = formr_aggregate(item_list = items, results = results, 
	compute_alphas = FALSE, plot_likert = FALSE)
agg[, c('religiousness', 'prefer')]
}
