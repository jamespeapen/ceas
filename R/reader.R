#' Read Seahorse Wave Excel File
#'
#' Reads input seahore data from an excel Seahorse Wave File
#' @param rep_list A list of Seahorse Wave excel export files. One file per
#' replicate. If your data is in a directory called "seahorse_data", use
#' `list.files("seahorse_data", pattern = "*.xlsx", full.names = TRUE)` to make
#' a list of the excel files.
#' @param sheet The number of the excel sheet containing the long-form Seahorse
#' data. Default is 2 because the long-form output from Seahorse Wave is on
#' sheet 2
#' @return a seahorse_rates table
#'
#' @importFrom data.table setDT := tstrsplit rbindlist
#' @importFrom readxl read_excel
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' head(seahorse_rates, n = 10)
read_data <- function(rep_list, sheet = 2) {
  data_cols <- c(
    "Measurement",
    "Group",
    "OCR",
    "ECAR",
    "PER"
  )
  reps <- lapply(seq.int(length(rep_list)), function(i) {
    # sanity check
    rep.i <- read_excel(rep_list[i], sheet)
    missing_cols <- setdiff(data_cols, colnames(rep.i))
    if (length(missing_cols) != 0) {
      stop(paste0("'", missing_cols, "'", " column was not found\n"))
    }

    # setup columns for partitioning
    Group <- NULL # suppress "no visible binding for global variable" error

    setDT(rep.i)[
      , c("exp_group", "assay_type") := tstrsplit(Group, " ", fixed = TRUE)
    ][, replicate := i][, Group := NULL]
  })
  rbindlist(reps)
}
