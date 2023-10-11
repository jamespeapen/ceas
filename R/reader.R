#' Read Seahorse Wave Excel File
#'
#' Reads input seahore data from an excel Seahorse Wave File
#' @param rep_list A list of Seahorse Wave excel export files. One file per
#' replicate. Group all replicates for a given experiment in a single folder,
#' and write that folder's path in "seahorse_data". You can use
#' `list.files("seahorse_data") "full.names=TRUE") to get the paths to the
#' files.
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
#' rep_list <- system.file("extdata", package = "ceas") |> list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' knitr::kable(head(seahorse_rates, n = 10), "simple")
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
      , c("cell_line", "assay_type") := tstrsplit(Group, " ", fixed = TRUE)
    ][, replicate := i][, Group := NULL]
  })
  rbindlist(reps)
}
