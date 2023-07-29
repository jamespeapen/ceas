#' Organize Seahorse Data
#'
#' Organizes Seahorse OCR and ECAR rates based on defined points during the
#' experiment (e.g. basal OCR before drug addition) - togglable in function if
#' you are modifying Mito Stress Test and Glyco Stress Test measurement numbers
#' (i.e. from 3 measurements to X measurements): TODO
#'
#' @param seahorse_rates data.table Seahorse OCR and ECAR rates (imported using `read_data` function)
#' @return a list of TODO(groups) from the data
#'
#' @export
#'
#' @examples rep_list <- list.files("result_dir", pattern = "*.xlsx",
#' full.names=TRUE) seahorse_rates <- read_data(rep_list, sheet=2)
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


#' Calculate ATP Production from OXPHOS and Glycolysis
#'
#' Calculates ATP production from glycolysis and OXPHOS at points defined in patitioned_data
#' @param partitioned_data a data.table of organized Seahorse OCR and ECAR
#' rates based on TODO(defined points). Returned by `partition_data`
#' @param ph pH value for energetics calculation (for XF Media, 7.5)
#' @param pka pKa value for energetics calculation (for XF Media, 6.063)
#' @param buffer buffer for energetics calculation (for XF Media, 0.1 mpH/pmol H+)
#' @return a `data.table` of glycolysis and OXPHOS rates by no drug vs drug treatments: TODO
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' rep_list <- list.files("result_dir", pattern = "*.xlsx", full.names=TRUE)
#' seahorse_rates <- read_data(rep_list, sheet=2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics_list <- get_energetics(partitioned_data, ph, pka, buffer)


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

  glyc_no_drugs <- (ppr_glyc_no_drugs * 1) + (basal_mito_resp * 0.167 * 2)
  glyc_max_ox <- (ppr_glyc_max_ox * 1) + (max_mito_resp * 0.167 * 2)
  glyc_max_glyc <- (ppr_glyc_max_glyc * 1) + (max_glyc * 0.167 * 2)

  ox_no_drugs <- ((ocr_coupled_no_drugs * 2.486) + (basal_mito_resp * 0.121)) * 2
  ox_max_ox <- ((ocr_coupled_max_ox * 2.486) + (max_mito_resp * 0.121)) * 2
  ox_max_glyc <- ((ocr_coupled_max_glyc * 2.486) + (max_glyc * 0.121)) * 2

  total_no_drugs <- ox_no_drugs + glyc_no_drugs
  total_max_ox <- ox_max_ox + glyc_max_ox
  total_max_glyc <- ox_max_glyc + glyc_max_glyc

  glyc_ind_no_drugs <- glyc_no_drugs / total_no_drugs * 100
  glyc_ind_max_ox <- glyc_max_ox / total_max_ox * 100
  glyc_ind_max_glyc <- glyc_max_glyc / total_max_glyc * 100
  gi_max <- glyc_max_glyc / (total_max_glyc + ox_max_ox) * 100

  bioenergetic_scope_no_drugs <- glyc_no_drugs * ox_no_drugs
  bioenergetic_scope_gi_max <- total_max_glyc * ox_max_ox

  ox_index_no_drugs <- ox_no_drugs / total_no_drugs * 100
  ox_index_max_ox <- ox_max_ox / total_max_ox * 100
  ox_index_max_glyc <- ox_max_glyc / total_max_glyc * 100

  seahorse_condition <- factor(partitioned_data$basal$Replicate)
  cell_line <- factor(partitioned_data$basal$cell_line)

  data.table(cell_line,
    glyc_no_drugs,
    glyc_max_ox,
    glyc_max_glyc,
    ox_no_drugs,
    ox_max_ox,
    ox_max_glyc)
}


#' Calculate ATP Production Mean and Standard Deviation
#'
#' Calculates mean and standard deviation of ATP production from glycolysis and
#' OXPHOS at points defined in `partition_data` and with values calculated
#' using the `get_energetics` function
#' @param seahorse_rates a data.table of Seahorse OCR and ECAR rates (from `get_energetics`)
#' @return a list of groups from the data
#'
#' @importFrom data.table data.table
#' @importFrom data.table .SD
#' @export
#'
#' @examples
#' rep_list <- list.files("result_dir", pattern = "*.xlsx", full.names=TRUE)
#' #' seahorse_rates <- read_data(rep_list, sheet=2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics_list <- get_energetics(partitioned_data, ph, pka, buffer)
#' energetics_summary <- get_energetics_summary(energetics_list)

get_energetics_summary <- function(energetics) {
  sdcols <- colnames(energetics)[-1]
  merge(
    energetics[, .(count = .N), by = cell_line],
    energetics[, as.list(unlist( # seems to be the way to get mean and sd as columns instead of rows: https://stackoverflow.com/a/29907103
        lapply(.SD, function(x) list(mean = mean(x), sd =  sd(x)))
    )), .SDcols = sdcols, by = cell_line]
  )
}


