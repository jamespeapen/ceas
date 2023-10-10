#' Organize Seahorse Data
#'
#' Organizes Seahorse OCR and ECAR rates based on defined time points (i.e. the
#' Measurement column) during the experiment. This time point can be specified
#' if you are modifying the Mito and Glyco Stress Test (i.e. from 3 measurements
#' per cycle to X measurements)
#' @param seahorse_rates data.table Seahorse OCR and ECAR rates (imported using `read_data` function)
#' @param basal_tp Basal respiration time point. Must be less than `uncoupled_tp`
#' @param uncoupled_tp ATP-coupled respiration time point. Must be less than `maxresp_tp`
#' @param maxresp_tp Maximal uncoupled respiration time point. Must be less than `nonmito_tp`
#' @param nonmito_tp Non-mitochondrial respiration time point. Must be larger than `maxresp_tp`
#' @param no_glucose_glyc_tp No glucose added acidification time point. Must be less than `glucose_glyc_tp`
#' @param glucose_glyc_tp Glucose-associated acidification time point. Must be less than `max_glyc_tp`
#' @param max_glyc_tp Maximal acidification time point. Must be less than `twodg_glyc_tp`
#' @param twodg_glyc_tp Non-glycolytic acidification time point. Must be larger than `max_glyc_tp`
#' @return a list of named time points from each assay cycle
#'
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |> list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
partition_data <- function(
    seahorse_rates,
    basal_tp = 3,
    uncoupled_tp = 6,
    maxresp_tp = 8,
    nonmito_tp = 12,
    no_glucose_glyc_tp = 3,
    glucose_glyc_tp = 6,
    max_glyc_tp = 8,
    twodg_glyc_tp = 12) {
  # suppress "no visible binding for global variable" error
  Measurement <- NULL
  assay_type <- NULL

  list(
    # Mito Stress Test Variables
    basal = seahorse_rates[Measurement == 3 & assay_type == "MITO"],
    uncoupled = seahorse_rates[Measurement == 6 & assay_type == "MITO"],
    maxresp = seahorse_rates[Measurement == 8 & assay_type == "MITO"],
    nonmito = seahorse_rates[Measurement == 12 & assay_type == "MITO"],
    # Glyco Stress Test Variables
    no_glucose_glyc = seahorse_rates[Measurement == 3 & assay_type == "GLYCO"],
    glucose_glyc = seahorse_rates[Measurement == 6 & assay_type == "GLYCO"],
    max_glyc = seahorse_rates[Measurement == 8 & assay_type == "GLYCO"],
    twodg_glyc = seahorse_rates[Measurement == 12 & assay_type == "GLYCO"]
  )
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
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |> list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics_list <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
get_energetics <- function(partitioned_data, ph, pka, buffer) {
  # BASAL CONDITIONS: +glucose, no drugs
  basal_mito_resp <- partitioned_data$basal$OCR - partitioned_data$nonmito$OCR
  uncoupled_mito_resp <- partitioned_data$uncoupled$OCR - partitioned_data$nonmito$OCR
  coupled_mito_resp <- (basal_mito_resp - uncoupled_mito_resp) * 0.908 # hyperpolarization constant
  no_glucose_glyc_acidification <- partitioned_data$no_glucose_glyc$ECAR - partitioned_data$twodg_glyc$ECAR
  glucose_glyc_acidification <- partitioned_data$glucose_glyc$ECAR - partitioned_data$twodg_glyc$ECAR

  # basal proton production rates (PPR)
  ppr_basal <- glucose_glyc_acidification / buffer
  ppr_basal_resp <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * basal_mito_resp
  ppr_basal_glyc <- ppr_basal - ppr_basal_resp

  # basal ATP calculations
  ATP_basal_glyc <- (ppr_basal_glyc * 1) + (basal_mito_resp * 2 * 0.167)
  ATP_basal_resp <- (coupled_mito_resp * 2 * 2.486) + (basal_mito_resp * 2 * 0.121)

  # MAX CONDITIONS: +glucose, +drugs, different between Mito and Glyco Stress Tests
  max_mito_resp <- partitioned_data$maxresp$OCR - partitioned_data$nonmito$OCR
  max_mito_acidification <- partitioned_data$maxresp$ECAR - partitioned_data$nonmito$ECAR
  max_glyc_acidification <- partitioned_data$max_glyc$ECAR - partitioned_data$twodg_glyc$ECAR
  max_glyc_resp <- partitioned_data$max_glyc$OCR - partitioned_data$nonmito$OCR

  # max proton production rates (PPR)
  ppr_max <- max_glyc_acidification / buffer
  ppr_max_resp <- (10^(ph - pka)) / (1 + (10^(ph - pka))) * 1 * max_glyc_resp
  ppr_max_glyc <- ppr_max - ppr_max_resp

  # max ATP calculations
  ATP_max_glyc <- (ppr_max_glyc * 1) + (max_glyc_resp * 2 * 0.167)
  ATP_max_resp <- (coupled_mito_resp * 2 * 2.486) + (max_mito_resp * 2 * 0.121)

  seahorse_condition <- factor(partitioned_data$basal$Replicate)
  cell_line <- factor(partitioned_data$basal$cell_line)

  data.table(
    cell_line,
    ATP_basal_glyc,
    ATP_max_glyc,
    ATP_basal_resp,
    ATP_max_resp
  )
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
#' rep_list <- system.file("extdata", package = "ceas") |> list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics_list <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
#' energetics_summary <- get_energetics_summary(energetics_list)
get_energetics_summary <- function(
    energetics,
    error_metric = "ci",
    conf_int = 0.95) {
  # suppress "no visible binding for global variable" error
  . <- NULL
  .N <- NULL
  cell_line <- NULL
  se <- NULL

  z_value <- qnorm(((1 - conf_int) / 2), lower.tail = FALSE)
  sdcols <- colnames(energetics)[-1]
  merge(
    energetics[, .(count = .N), by = cell_line],
    energetics[, as.list(unlist( # seems to be the way to get mean and sd as columns instead of rows: https://stackoverflow.com/a/29907103
      lapply(
        .SD,
        function(x) {
          list(
            mean = mean(x),
            sd = sd(x),
            se = se(x),
            lower_bound = ifelse(
              error_metric == "sd",
              mean(x) - sd(x),
              mean(x) - (z_value * se(x))
            ),
            higher_bound = ifelse(
              error_metric == "sd",
              mean(x) + sd(x),
              mean(x) + (z_value * se(x))
            )
          )
        }
      )
    )), .SDcols = sdcols, by = cell_line]
  )
}

se <- function(x) {
  sd(x) / sqrt(length(x))
}
