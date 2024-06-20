#' Read Seahorse Wave Excel File
#'
#' Reads input seahore data from an excel Seahorse Wave File. It  assumes your
#' data is background normalized.
#' @param rep_list A list of Seahorse Wave excel export files. One file per
#' replicate. If your data is in a directory called "seahorse_data", use
#' `list.files("seahorse_data", pattern = "*.xlsx", full.names = TRUE)` to make
#' a list of the excel files. Add multiple replicates with care - see details.
#' @param norm A csv file with the experimental groups and their normalization
#' values. Leave unset if normalization is not required. See [normalize()].
#' @param sheet The number of the excel sheet containing the long-form Seahorse
#' data. Default is 2 because the long-form output from Seahorse Wave is on
#' sheet 2
#' @param delimiter The delimiter between the group name and the assay type in
#' the Group column of the wave output. e.g. "Group1 MITO" would use a space
#' character as delimiter.
#' @return a seahorse_rates table
#'
#' @details
#' Although ceas enables integration of multiple biological and/or technical
#' replicates, previous work has reported high inter-plate variation (Yepez et.
#' al 2018). If you don't want your replicate data combined, make sure that the
#' names of the common groups between the replicates are different.
#'
#' @references
#' Yépez \emph{et al.} 2018
#' \emph{OCR-Stats: Robust estimation and statistical testing of mitochondrial
#' respiration activities using Seahorse XF Analyzer}
#' \emph{PLOS ONE} 2018;\strong{13}:e0199938.
#' \doi{10.1371/journal.pone.0199938}
#'
#' @importFrom data.table setDT := tstrsplit rbindlist
#' @importFrom readxl read_excel
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' head(seahorse_rates, n = 10)
#'
#' # normalization
#' norm_csv <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "norm.csv", full.names = TRUE)
#' seahorse_rates.norm <- read_data(rep_list, norm = norm_csv, sheet = 2)
#' head(seahorse_rates.norm, n = 10)
read_data <- function(rep_list, norm = NULL, sheet = 2, delimiter = " ") {
  # suppress "no visible binding for global variable" error
  exp_group <- NULL
  Group <- NULL
  ECAR <- NULL
  OCR <- NULL
  PER <- NULL
  . <- NULL

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
    setDT(rep.i)[
      , c("exp_group", "assay_type") := tstrsplit(Group, delimiter, fixed = TRUE)
    ][, replicate := i][, Group := NULL]
  })
  rates_dt <- rbindlist(reps)

  norm_warning <- "'Background' is not 0 in the rows shown below - check that background normalization was performed by Wave\n"
  background_dt <- rates_dt[exp_group == "Background"]
  if (!all(background_dt[, .(OCR, ECAR, PER)] == 0)) {
    warning(
      norm_warning,
      paste(
        capture.output(rates_dt[exp_group == "Background"][OCR != 0 | ECAR != 0 | PER != 0]),
        collapse = "\n"
      )
    )
  }

  if (!is.null(norm)) {
    rates_dt <- normalize(rates_dt, norm)
  }

  rates_dt
}
