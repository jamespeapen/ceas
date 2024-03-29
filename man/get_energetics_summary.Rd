% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energetics.R
\name{get_energetics_summary}
\alias{get_energetics_summary}
\title{Calculate ATP Production Mean and Standard Deviation}
\usage{
get_energetics_summary(energetics, error_metric = "ci", conf_int = 0.95)
}
\arguments{
\item{energetics}{a data.table of Seahorse OCR and ECAR rates (from \code{get_energetics})}

\item{error_metric}{Whether to calculate error as standard deviation (\code{"sd"}) or confidence intervals (\code{"ci"})}

\item{conf_int}{The confidence interval percentage. Should be between 0 and 1}
}
\value{
a list of groups from the data
}
\description{
Calculates mean and standard deviation of ATP production from glycolysis and
OXPHOS at points defined in \code{partition_data} and with values calculated
using the \code{get_energetics} function
}
\examples{
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
energetics_list <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
energetics_summary <- get_energetics_summary(energetics_list)
head(energetics_summary[, c(1:5)], n = 10)
head(energetics_summary[, c(1, 2, 6, 7)], n = 10)
}
