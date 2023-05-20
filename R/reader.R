#' Input data TODO
#'
#' Reads input data
#' @param rep_list A list of paths to the excel files. One file per replicate
#' @param sheet The number of the sheet containing the data. Default is 2
#' because the output from Seahorse Wave is on sheet 2
#' @return a seahorse_rates table
#'
#' @importFrom data.table setDT := tstrsplit rbindlist
#' @importFrom readxl read_excel
#' @export
#'
#' @examples
#' seahorse_rates <- read_data("sample1")

read_data <- function(rep_list, sheet = 2) {
  # TODO: sanity check for column names
  reps <- lapply(seq.int(length(rep_list)), function(i) {
    setDT(read_excel(rep_list[i], sheet))[
    , c("cell_line", "assay_type") := tstrsplit(Group, " ", fixed = TRUE)
    ][, replicate := i][, Group := NULL]
  })
  rbindlist(reps)
}

