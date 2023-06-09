% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reader.R
\name{read_data}
\alias{read_data}
\title{Input data TODO}
\usage{
read_data(rep_list, sheet = 2)
}
\arguments{
\item{rep_list}{A list of paths to the excel files. One file per replicate}

\item{sheet}{The number of the sheet containing the data. Default is 2
because the output from Seahorse Wave is on sheet 2}
}
\value{
a seahorse_rates table
}
\description{
Reads input data
}
\examples{
seahorse_rates <- read_data("sample1")
}
