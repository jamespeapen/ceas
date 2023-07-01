#' Read seahorse data
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
#' replicate_list <- list.files("result_dir", pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_seahorse(rep_list)

read_seahorse <- function(rep_list, sheet = 2) {

  # TODO: are these the defaults from seahorse? can they be user defined?
  data_cols <- c(
    "Measurement",
    "Group",
    "OCR",
    "ECAR",
    "PERtexlive.combined.scheme-basic"
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
    , c("cell_line", "assay_type") := tstrsplit(Group, " ", fixed = TRUE)][
    , replicate := i][
    , Group := NULL]
  })

  rbindlist(reps)
}

