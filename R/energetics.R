#' Get energetics list
#'
#' Makes list of energetics tables
#' @param seahorse_rates data.table of the input rates
#' @return a list of groups from the data
#'
#' @export
#'
#' @examples
#' replicate_list <- c("rep1.xlsx", "rep2.xlsx", "rep3.xlsx")
#' seahorse_rates <- read_data(replicate_list)
#' partitioned_data <- partition_data(seahorse_rates)

partition_data <- function(seahorse_rates) {
  list(
    basal = subset(seahorse_rates, Measurement == 3 & assay_type == "MITO"),
    uncoupled = subset(seahorse_rates, Measurement == 6 & assay_type == "MITO"),
    maxresp = subset(seahorse_rates, Measurement == 8 & assay_type == "MITO"),
    nonmito = subset(seahorse_rates, Measurement == 12 & assay_type == "MITO"),
    maxgly = subset(seahorse_rates, Measurement == 6 & assay_type == "MITO"),
    fccp_ecar = subset(seahorse_rates, Measurement == 8 & assay_type == "MITO"),
    basal_ecar = subset(seahorse_rates, Measurement == 3 & assay_type == "GLYCO"),
    oligomon_ecar = subset(seahorse_rates, Measurement == 8 & assay_type == "GLYCO")
  )
}

#' Get energetics
#'
#' Makes table of energetics
#' @param seahorse_rates data.table of the input rates
#' @return a list of groups from the data
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' seahorse_rates <- read_data(seahorse_rates)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics <- get_energetics(partitioned_data)

get_energetics <- function(partitioned_data, ph, pka, buffer) {
  basal_mito_resp <- partitioned_data$basal$OCR - partitioned_data$nonmito$OCR
  max_mito_resp <- partitioned_data$maxresp$OCR - partitioned_data$nonmito$OCR

  max_glyc <- partitioned_data$maxgly$ECAR - partitioned_data$nonmito$OCR

  ocr_coupled_no_drugs <- (partitioned_data$basal$OCR - partitioned_data$uncoupled$OCR) / 0.908
  ocr_coupled_max_ox <- max_mito_resp
  ocr_coupled_max_glyc <- max_glyc

  ppr_total_no_drugs <- partitioned_data$basal_ecar$ECAR / buffer
  ppr_total_max_ox <- partitioned_data$fccp_ecar$ECAR / buffer
  ppr_total_max_glyc <- partitioned_data$oligomon_ecar$ECAR / buffer

  ppr_resp_no_drugs <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * basal_mito_resp
  ppr_resp_max_ox <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * max_mito_resp
  ppr_resp_max_glyc <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * max_glyc

  ppr_glyc_no_drugs <- ppr_total_no_drugs - ppr_resp_no_drugs
  ppr_glyc_max_ox <- ppr_total_max_ox - ppr_resp_max_ox
  ppr_glyc_max_glyc <- ppr_total_max_glyc - ppr_resp_max_glyc

  jatp_glyc_no_drugs <- (ppr_glyc_no_drugs * 1) + (basal_mito_resp * 0.167 * 2)
  jatp_glyc_max_ox <- (ppr_glyc_max_ox * 1) + (max_mito_resp * 0.167 * 2)
  jatp_glyc_max_glyc <- (ppr_glyc_max_glyc * 1) + (max_glyc * 0.167 * 2)

  jatp_ox_no_drugs <- ((ocr_coupled_no_drugs * 2.486) + (basal_mito_resp * 0.121)) * 2
  jatp_ox_max_ox <- ((ocr_coupled_max_ox * 2.486) + (max_mito_resp * 0.121)) * 2
  jatp_ox_max_glyc <- ((ocr_coupled_max_glyc * 2.486) + (max_glyc * 0.121)) * 2

  jatp_total_no_drugs <- jatp_ox_no_drugs + jatp_glyc_no_drugs
  jatp_total_max_ox <- jatp_ox_max_ox + jatp_glyc_max_ox
  jatp_total_max_glyc <- jatp_ox_max_glyc + jatp_glyc_max_glyc

  glyc_ind_no_drugs <- jatp_glyc_no_drugs / jatp_total_no_drugs * 100
  glyc_ind_max_ox <- jatp_glyc_max_ox / jatp_total_max_ox * 100
  glyc_ind_max_glyc <- jatp_glyc_max_glyc / jatp_total_max_glyc * 100
  gi_max <- jatp_glyc_max_glyc / (jatp_total_max_glyc + jatp_ox_max_ox) * 100

  bioenergetic_scope_no_drugs <- jatp_glyc_no_drugs * jatp_ox_no_drugs
  bioenergetic_scope_gi_max <- jatp_total_max_glyc * jatp_ox_max_ox

  ox_index_no_drugs <- jatp_ox_no_drugs / jatp_total_no_drugs * 100
  ox_index_max_ox <- jatp_ox_max_ox / jatp_total_max_ox * 100
  ox_index_max_glyc <- jatp_ox_max_glyc / jatp_total_max_glyc * 100

  seahorse_condition <- factor(partitioned_data$basal$Replicate)
  seahorse_cell_line <- factor(partitioned_data$basal$cell_line)

  data.table(seahorse_cell_line,
    jatp_glyc_no_drugs,
    jatp_glyc_max_ox,
    jatp_glyc_max_glyc,
    jatp_ox_no_drugs,
    jatp_ox_max_ox,
    jatp_ox_max_glyc)
}

#' Get energetics summary
#'
#' Makes table of energetics
#' @param seahorse_rates data.table of the input rates
#' @return a list of groups from the data
#'
#' @importFrom data.table data.table
#' @importFrom data.table .SD
#' @export
#'
#' @examples
#' seahorse_rates <- read_data(seahorse_rates)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics <- get_energetics(partitioned_data)
#' energetics_summary <- get_energetics_summary(energetics_list)

get_energetics_summary <- function(energetics) {
  sdcols <- colnames(energetics)[-1]
  merge(
    energetics[, .(count = .N), by = seahorse_cell_line],
    energetics[, as.list(unlist( # seems to be the way to get mean and sd as columns instead of rows: https://stackoverflow.com/a/29907103
        lapply(.SD, function(x) list(mean = mean(x), sd =  sd(x)))
    )), .SDcols = sdcols, by = seahorse_cell_line]
  )
}

