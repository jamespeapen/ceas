#' Normalize Seahorse data
#'
#' Normalizes input data according to a sample normalization measure (e.g. cell
#' number or \eqn{\mu}g of protein). It assumes your data is background
#' normalized.
#' @param seahorse_rates The seahorse rates table read by the [read_data()]
#' function.
#' @param norm_csv A csv file with either well or experimental group in column
#' 1 and the sample normalization measure in column 2. Headers are ignored.
#' @param norm_column Whether to normalize by `"well"` or `"exp_group"` group.
#' The first column of the normalization csv provided should match this value.
#' @param norm_method How to normalize each well or experimental group (specified by `norm_column`):
#'  - by its corresponding row in the `norm_csv` (`"self"`) or
#'  - by the minimum of the `measure` column in the provided `norm_csv` (`"minimum"`).
#'
#' See details.
#'
#' @return a normalized seahorse_rates data.table
#'
#' @details
#'
#' This normalization is distinct from the background normalization done by the
#' Wave software. If the data are not background normalized, `read_data()` will
#' output a warning showing rows with OCR, ECAR and PER values greater than 0.
#'
#' ### Normalization Methods
#'
#' When `norm_method` is set to `"self"`, each OCR, ECAR, and PER value is
#' divided by the measure it"self". OCR and ECAR values are divided by the
#' corresponding raw value in the "measure" column: an intra-well or
#' experimental group normalization. Each normalized value is then interpreted
#' as pmol/min per measure (e.g. pmol/min/cell  or pmol/min/\eqn{\mu}g of
#' protein.
#'
#' When set to `"minimum"`, each OCR, ECAR, and PER value is normalized by the
#' minimum value in the `norm_csv` "measure" column. In this method, every
#' "measure" column's value in the provided CSV file is divided by the lowest
#' of the "measure" values to get a normalization factor for each well or
#' experimental group. The OCR, ECAR, and PER values in each well or
#' experimental group are divided by their corresponding normalization factors.
#' Compared to `"self"`, this is an inter-well/experimental group normalization
#' based on the lowest `"measure"`. The results may be interpreted as pmol/min
#' per minimum of the measure (eg: group cell count or \eqn{\mu}g of protein.)
#'
#'
#' @importFrom data.table fread := copy
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' norm_csv <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "^norm.csv", full.names = TRUE)
#' read.csv(norm_csv)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' head(seahorse_rates, n = 10)
#' # normalize by experimental group based on the minimum cell count or protein quantity
#' seahorse_rates.normalized <- normalize(
#'   seahorse_rates,
#'   norm_csv,
#'   norm_column = "exp_group",
#'   norm_method = "minimum"
#' )
#' head(seahorse_rates.normalized, n = 10)
normalize <- function(seahorse_rates, norm_csv, norm_column = "well", norm_method = "minimum") {
  i.norm_const <- NULL
  i.measure <- NULL
  norm_const <- NULL
  measure <- NULL
  ECAR <- NULL
  OCR <- NULL
  PER <- NULL

  norm_columns <- c("well", "exp_group")
  norm_methods <- c("self", "minimum")

  stopifnot("'norm_column' should be 'well' or 'exp_group'" = tolower(norm_column) %in% c(norm_columns))
  stopifnot("'norm_method' should be 'self' or 'minimum'" = norm_method %in% c(norm_methods))

  if (norm_column == "exp_group" & missing(norm_column)) warning(norm_column_warning)
  if (norm_method == "minimum" & missing(norm_method)) warning(norm_method_warning)

  well_group_column <- ifelse(tolower(norm_column) == "well", "Well", "exp_group")
  method <- ifelse(norm_method == "self", "self", "minimum")

  norm_dt <- fread(norm_csv, col.names = c(well_group_column, "measure"))
  norm_dt[measure == 0, measure := NA][, norm_const := measure / min(measure, na.rm = TRUE)]

  normalized_rates <- copy(seahorse_rates)
  if (method == "self") {
    (
      normalized_rates[norm_dt, on = well_group_column, measure := i.measure]
      [, `:=`(
          ECAR = ifelse(is.na(measure), ECAR, ECAR / measure),
          OCR = ifelse(is.na(measure), OCR, OCR / measure),
          PER = ifelse(is.na(measure), PER, PER / measure)
        )]
      [, measure := NULL][]
    )
  } else {
    (
      normalized_rates[norm_dt, on = well_group_column, norm_const := i.norm_const]
      [, `:=`(
          ECAR = ifelse(ECAR == 0, ECAR, ECAR / norm_const),
          OCR = ifelse(OCR == 0, OCR, OCR / norm_const),
          PER = ifelse(PER == 0, PER, PER / norm_const)
        )]
      [, norm_const := NULL][]
    )
  }
}
