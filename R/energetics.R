#' Organize Seahorse Data
#'
#' Organizes Seahorse OCR and ECAR rates based on defined time points (i.e. the
#' Measurement column) during the experiment. This time point can be specified
#' if you are modifying the Mito and Glyco Stress Test (i.e. from 3 measurements
#' per cycle to X measurements)
#' @param seahorse_rates A data.table of OCR and ECAR rates returned by `read_data`
#' @param assay_types A list that configures data partitioning based on the type of assay. See details.
#' @param basal_tp Basal respiration time point. Must be less than `uncoupled_tp`
#' @param uncoupled_tp ATP-coupled respiration time point. Must be less than `maxresp_tp`
#' @param maxresp_tp Maximal uncoupled respiration time point. Must be less than `nonmito_tp`
#' @param nonmito_tp Non-mitochondrial respiration time point. Must be larger than `maxresp_tp`
#' @param no_glucose_glyc_tp No glucose added acidification time point. Must be less than `glucose_glyc_tp`
#' @param glucose_glyc_tp Glucose-associated acidification time point. Must be less than `max_glyc_tp`
#' @param max_glyc_tp Maximal acidification time point. Must be less than `twodg_glyc_tp`
#' @details
#' `partition_data` sets up the rates data for ATP calculations by the
#' `get_energetics` function. To do this, it takes a list `assay_types` with
#' the named values `basal`, `uncoupled`, `maxresp`, `nonmito`,
#' `no_glucose_glyc`, `glucose_glyc`, and `max_glyc`. In the default setting,
#' it is configured for an experiment with both Mito and Glyco assays. However,
#' partitioning can be configured for other experimental conditions.
#' * Only MITO data:
#'
#' \preformatted{partitioned_data <- partition_data(
#'   seahorse_rates,
#'   assay_types = list(
#'     basal = "MITO",
#'     uncoupled = "MITO",
#'     maxresp = "MITO",
#'     nonmito = "MITO",
#'     no_glucose_glyc = NA,
#'     glucose_glyc = "MITO",
#'     max_glyc = "MITO"
#'   ),
#'   basal_tp = 3,
#'   uncoupled_tp = 6,
#'   maxresp_tp = 8,
#'   nonmito_tp = 12,
#'   no_glucose_glyc_tp = NA,
#'   glucose_glyc_tp = 3,
#'   max_glyc_tp = 6
#' )
#' }
#'
#' * Data according to Mookerjee \emph{et al.} 2017 \emph{J Biol Chem};\emph{292}:7189-207.
#'
#' \preformatted{partitioned_data <- partition_data(
#'   seahorse_rates,
#'   assay_types = list(
#'     basal = "MITO",
#'     uncoupled = "MITO",
#'     maxresp = NA,
#'     nonmito = "MITO",
#'     no_glucose_glyc = NA,
#'     glucose_glyc = "MITO",
#'     max_glyc = "MITO"
#'   ),
#'   basal_tp = 5,
#'   uncoupled_tp = 10,
#'   maxresp_tp = NA,
#'   nonmito_tp = 12,
#'   no_glucose_glyc_tp = 2,
#'   glucose_glyc_tp = 5,
#'   max_glyc_tp = 10
#' )
#' }
#'
#' Also see the vignette.
#'
#' @return a list of named time points from each assay cycle
#'
#' @importFrom data.table setkey
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
partition_data <- function(
    seahorse_rates,
    assay_types = list(
      basal = "MITO",
      uncoupled = "MITO",
      maxresp = "MITO",
      nonmito = "MITO",
      no_glucose_glyc = "GLYCO",
      glucose_glyc = "GLYCO",
      max_glyc = "GLYCO"
    ),
    basal_tp = 3,
    uncoupled_tp = 6,
    maxresp_tp = 8,
    nonmito_tp = 12,
    no_glucose_glyc_tp = 3,
    glucose_glyc_tp = 6,
    max_glyc_tp = 8) {
  # suppress "no visible binding for global variable" error
  Measurement <- NULL
  assay_type <- NULL
  exp_group <- NULL

  partitioned_data <- list(
    # Mito Stress Test Variables
    basal = seahorse_rates[Measurement == basal_tp & assay_type == assay_types$basal],
    uncoupled = seahorse_rates[Measurement == uncoupled_tp & assay_type == assay_types$uncoupled],
    maxresp = seahorse_rates[Measurement == maxresp_tp & assay_type == assay_types$maxresp],
    nonmito = seahorse_rates[Measurement == nonmito_tp & assay_type == assay_types$nonmito],
    # Glyco Stress Test Variables
    no_glucose_glyc = seahorse_rates[Measurement == no_glucose_glyc_tp & assay_type == assay_types$no_glucose_glyc],
    glucose_glyc = seahorse_rates[Measurement == glucose_glyc_tp & assay_type == assay_types$glucose_glyc],
    max_glyc = seahorse_rates[Measurement == max_glyc_tp & assay_type == assay_types$max_glyc]
  )

  # set a key on the data.tables so they are ordered correctly during the energetics calculations
  lapply(partitioned_data, function(DT) setkey(DT, exp_group, assay_type))
}

#' Calculate ATP Production from OXPHOS and Glycolysis
#'
#' Calculates ATP production from glycolysis and OXPHOS at points defined in patitioned_data
#'
#' @details
#' TODO: check that all symbols are defined
#'
#' Proton production rate (PPR):
#' \deqn{\text{PPR} = \frac{\text{ECAR value}}{\text{buffer}}}
#'
#' \deqn{
#'   \text{PPR}_{\text{mito}} = \frac{10^{\text{pH}-\text{pK}_a}}{1+10^{\text{pH}-\text{pK}_a}} \cdot \frac{\text{H}^+}{\text{O}_2} \cdot \text{OCR}
#' }
#'
#' calculates the proton production from glucose during its conversion to bicarbonate and \eqn{\text{H}^+} assuming max \eqn{\frac{\text{H}^+}{\text{O}_2}} of 1
#'
#' \deqn{
#'  \text{PPR}_\text{glyc} = \text{PPR} - \text{PPR}_\text{resp}
#' }
#'
#' calculates the proton production from glucose during its conversion to lactate + \eqn{\text{H}^+}
#'
#' Joules of ATP (JATP) production:
#'
#' \deqn{
#'   \text{ATP}_{\text{glyc}} =
#'    \Bigl(\text{PPR}_\text{glyc} \cdot \frac{\text{ATP}}{\text{lactate}}\Bigl) +
#'    \Bigl(\text{MITO}_\text{resp} \cdot 2 \cdot \frac{\text{P}}{\text{O}_\text{glyc}}\Bigl)
#' }
#'
#' \deqn{
#'  \frac{\text{ATP}}{\text{lactate}} = 1
#' }
#' with
#' \eqn{\frac{\text{P}}{{\text{O}_\text{glyc}}}} = 0.167 for glucose (0.242 for glycogen).
#'
#' \deqn{
#'  \text{ATP}_\text{resp} =
#'   \Bigl(\text{coupled MITO}_\text{resp} \cdot 2 \cdot \frac{\text{P}}{\text{O}_\text{oxphos}}\Bigl) +
#'   \Bigl(\text{MITO}_\text{resp} \cdot 2 \cdot \frac{\text{P}}{\text{O}_\text{TCA}}\Bigl)
#' }
#' with \eqn{\frac{\text{P}}{{\text{O}_\text{oxphos}}}}  = 2.486 and \eqn{\frac{\text{P}}{{\text{O}_\text{TCA}}}} = 0.167.
#'
#' @param partitioned_data a data.table of organized Seahorse OCR and ECAR
#' rates based on timepoints from the assay cycle. Returned by `partition_data`
#' @param ph pH value for energetics calculation (for XF Media, 7.5)
#' @param pka pKa value for energetics calculation (for XF Media, 6.063)
#' @param buffer buffer for energetics calculation (for XF Media, 0.1 mpH/pmol H+)
#' @return a `data.table` of glycolysis and OXPHOS rates
#'
#' @importFrom data.table data.table setkey
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
#' head(energetics, n = 10)
get_energetics <- function(partitioned_data, ph, pka, buffer) {
  . <- NULL
  .I <- NULL
  ID <- NULL
  OCR <- NULL
  exp_group <- NULL
  assay_type <- NULL
  mean_non_mito <- NULL

  P_OTCA_RATIO_GLYCOGEN <- 0.121
  P_OGLYC_RATIO_GLUCOSE <- 0.167
  P_OOXPHOS_RATIO_GLYCOGEN <- 2.486
  HYPERPOLARIZATION_CONSTANT <- 0.908

  # BASAL CONDITIONS: +glucose, no drugs
  basal_mito_resp <- partitioned_data$basal$OCR - partitioned_data$nonmito$OCR
  uncoupled_mito_resp <- partitioned_data$uncoupled$OCR - partitioned_data$nonmito$OCR
  coupled_mito_resp <- (basal_mito_resp - uncoupled_mito_resp) / HYPERPOLARIZATION_CONSTANT
  glucose_glyc_acidification <- partitioned_data$glucose_glyc$ECAR

  mean_group_nonmito_resp <- partitioned_data$nonmito[, .(mean_non_mito = mean(OCR)), by = .(assay_type, exp_group)]

  # Subtract the mean_non_mito based on exp_group group
  glucose_glyc_resp <- partitioned_data$glucose_glyc[mean_group_nonmito_resp, on = .(exp_group)][, OCR - mean_non_mito]

  ppr_basal <- glucose_glyc_acidification / buffer
  ppr_basal_glyc_resp <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * glucose_glyc_resp
  ppr_basal_glyc <- ppr_basal - ppr_basal_glyc_resp

  # basal ATP calculations
  ATP_basal_glyc <- (ppr_basal_glyc * 1) + (glucose_glyc_resp * 2 * P_OGLYC_RATIO_GLUCOSE)
  ATP_basal_resp <- (coupled_mito_resp * 2 * P_OOXPHOS_RATIO_GLYCOGEN) + (basal_mito_resp * 2 * P_OTCA_RATIO_GLYCOGEN)

  # MAX CONDITIONS: +glucose, +drugs, different between Mito and Glyco Stress Tests
  max_mito_resp <- partitioned_data$maxresp$OCR - partitioned_data$nonmito$OCR
  max_glyc_acidification <- partitioned_data$max_glyc$ECAR

  # Subtract the mean_non_mito based on exp_group group
  max_glyc_resp <- partitioned_data$max_glyc[mean_group_nonmito_resp, on = .(exp_group)][, OCR - mean_non_mito]

  # max proton production rates (PPR)
  ppr_max <- max_glyc_acidification / buffer
  ppr_max_glyc_resp <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * max_glyc_resp
  ppr_max_glyc <- ppr_max - ppr_max_glyc_resp

  # max ATP calculations
  ATP_max_glyc <- (ppr_max_glyc * 1) + (max_glyc_resp * 2 * P_OGLYC_RATIO_GLUCOSE)
  ATP_max_resp <- (coupled_mito_resp * 2 * P_OOXPHOS_RATIO_GLYCOGEN) + (max_mito_resp * 2 * P_OTCA_RATIO_GLYCOGEN)

  exp_group_mito <- factor(partitioned_data$basal$exp_group)

  MITO_df <- data.table(
    exp_group = exp_group_mito,
    ATP_basal_resp,
    ATP_max_resp
  )

  exp_group_glyco <- factor(partitioned_data$glucose_glyc$exp_group)

  GLYCO_df <- data.table(
    exp_group = exp_group_glyco,
    ATP_basal_glyc,
    ATP_max_glyc
  )

  lapply(list(MITO_df, GLYCO_df), function(DT) {
    DT[, ID := paste(exp_group, .I, sep = "_")]
    setkey(DT, ID, exp_group)
  })

  merge(MITO_df, GLYCO_df, all.x = TRUE, all.y = TRUE)[, ID := NULL][]
}

#' Calculate ATP Production Mean and Standard Deviation
#'
#' Calculates mean and standard deviation of ATP production from glycolysis and
#' OXPHOS at points defined in `partition_data` and with values calculated
#' using the `get_energetics` function
#' @param energetics a data.table of Seahorse OCR and ECAR rates (from `get_energetics`)
#' @param error_metric Whether to calculate error as standard deviation (`"sd"`) or confidence intervals (`"ci"`)
#' @param conf_int The confidence interval percentage. Should be between 0 and 1
#' @return a list of groups from the data
#'
#' @importFrom data.table .SD
#' @importFrom stats sd
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics_list <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
#' energetics_summary <- get_energetics_summary(energetics_list)
#' head(energetics_summary[, c(1:5)], n = 10)
#' head(energetics_summary[, c(1, 2, 6, 7)], n = 10)
get_energetics_summary <- function(
    energetics,
    error_metric = "ci",
    conf_int = 0.95) {
  # suppress "no visible binding for global variable" error
  . <- NULL
  .N <- NULL
  exp_group <- NULL
  se <- NULL

  z_value <- qnorm(((1 - conf_int) / 2), lower.tail = FALSE)
  sdcols <- colnames(energetics)[-1]
  merge(
    energetics[, .(count = .N), by = exp_group],
    energetics[, as.list(unlist( # seems to be the way to get mean and sd as columns instead of rows: https://stackoverflow.com/a/29907103
      lapply(
        .SD,
        function(x) {
          list(
            mean = mean(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE),
            se = se(x),
            lower_bound = ifelse(
              error_metric == "sd",
              mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
              mean(x, na.rm = TRUE) - (z_value * se(x))
            ),
            higher_bound = ifelse(
              error_metric == "sd",
              mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE),
              mean(x, na.rm = TRUE) + (z_value * se(x))
            )
          )
        }
      )
    )), .SDcols = sdcols, by = exp_group]
  )
}

se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}
