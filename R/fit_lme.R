#' Estimate mean and confidence intervals using a mixed-effects model
#'
#' Estimates mean and standard deviation of energetics or rates with
#' replicates as the random-effect
#' @param data_col The column name of the ATP measure ("ATP_basal_resp",
#' "ATP_max_resp", "ATP_basal_glyc", "ATP_max_glyc") or rate measure ("OCR", "ECAR")
#' @param input The dataset containing `data_col` from `get_energetics` or `read_data`
#' @param group_colname The column containing experimental group names
#' @param rep_colname The column containing replicate IDs
#' @return an `lme4::lmer` mixed effects model
#'
#' @importFrom lme4 lmer
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
#' fit_lme("ATP_max_glyc", energetics)
fit_lme <- function(
    data_col,
    input,
    group_colname = "exp_group",
    rep_colname = "replicate") {
  lmer(
    as.formula(paste0(data_col, " ~ ", group_colname, " + (1 | ", rep_colname, ")")),
    data = input
  )
}
