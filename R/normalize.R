#' Normalize Seahorse data
#'
#' Normalizes input data according to cell number or \eqn{\mu}g of protein. It
#' assumes your data is background normalized.
#' @param seahorse_rates The seahorse rates table read by the [read_data()]
#' function.
#' @param norm_csv A csv file with the experimental groups in column 1 and cell
#' count or \eqn{\mu}g of protein in column 2. Headers are ignored.
#' @return a normalzed seahorse_rates data.table
#'
#' @importFrom data.table fread := copy
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' norm_csv <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "norm.csv", full.names = TRUE)
#' read.csv(norm_csv)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' seahorse_rates.normalized <- normalize(seahorse_rates, norm_csv)
#' head(seahorse_rates.normalized, n = 10)
normalize <- function(seahorse_rates, norm_csv) {
  i.norm_const <- NULL
  norm_const <- NULL
  measure <- NULL
  ECAR <- NULL
  OCR <- NULL
  PER <- NULL

  norm_dt <- fread(norm_csv, col.names = c("exp_group", "measure"))
  norm_dt[, norm_const := measure / min(measure)]

  (
    copy(seahorse_rates)
    [norm_dt, on = "exp_group", norm_const := i.norm_const]
    [, `:=`(
        ECAR = ifelse(ECAR == 0, ECAR, ECAR / norm_const),
        OCR = ifelse(OCR == 0, OCR, OCR / norm_const),
        PER = ifelse(PER == 0, PER, PER / norm_const)
      )]
    [, norm_const := NULL][]
  )
}
