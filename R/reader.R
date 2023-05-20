#' Input data TODO
#'
#' Reads input data
#' @param input_data The name of the sample file
#' @param sheet The number of the sheet containing the data. Default is 2
#' because the output from Seahorse Wave is on sheet 2
#' @return a seahorse_rates table
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @importFrom data.table tstrsplit
#' @importFrom readxl read_excel
#' @export
#'
#' @examples
#' seahorse_rates <- read_data("sample1")

read_data <- function(input_data, sheet = 2) {
  as.data.table(read_excel(input_data, sheet))[
  , c("cell_line", "assay_type") := tstrsplit( Group, " ", fixed = TRUE)
  ][]
}

