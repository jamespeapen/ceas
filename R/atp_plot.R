#' ATP Plot
#'
#' Generate the ATP Plot
#' @param energetics A table of calculated glycolysis and OXPHOS rates.
#' Returned by `get_energetics`
#' @param model The linear model used to estimate mean and confidence
#' intervals: ordinary least squares (`"ols"`) or mixed-effects (`"mixed"`)
#' @param error_bar Whether to plot error bars as standard deviation (`"sd"`)
#' or confidence intervals (`"ci"`)
#' @param conf_int The confidence interval percentage. Should be between 0 and 1
#' @param size Size of the points
#' @param shape Shape of the points
#' @param basal_vs_max Whether to plot `"basal"` or `"max"` respiration
#' @param glyc_vs_resp Whether to plot glycolysis (`"glyc"`)  or respiration (`"resp"`)
#' @param group_label Label for the experimental group to populate the legend title
#' @param sep_reps Whether to calculate summary statistics on the groups with
#' replicates combined. The current default `FALSE` combines replicates, but
#' future releases will default to `TRUE` providing replicate-specific
#' summaries.
#' @param ci_method The method used to compute confidence intervals for the
#' mixed-effects model: `"Wald"`, `"profile"`, or `"boot"` passed to
#' `lme4::confint.merMod()`.
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlab ylab geom_linerange xlim ylim geom_linerange theme_bw .data position_dodge2
#' @export
#'
#' @details
#'
#' **Note:**
#' When we use the term 'max' in the package documentation we mean the maximal
#' experimental OCR and ECAR values rather than absolute biological maximums.
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics <- get_energetics(
#'   partitioned_data,
#'   ph = 7.4,
#'   pka = 6.093,
#'   buffer = 0.1
#' )
#' atp_plot(energetics, sep_reps = FALSE)
#'
#' atp_plot(energetics, basal_vs_max = "max", sep_reps = FALSE)
#'
#' atp_plot(
#'   energetics,
#'   basal_vs_max = "basal",
#'   glyc_vs_resp = "resp",
#'   sep_reps = TRUE
#' )
#' # to change fill, the geom_point shape number should be between 15 and 25
#' atp_plot(
#'   energetics,
#'   sep_reps = FALSE
#' ) +
#'   ggplot2::scale_fill_manual(
#'     values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
#'   )
#'
#' # to change color, use ggplot2::scale_color_manual
#' atp_plot(energetics, sep_reps = FALSE) +
#'   ggplot2::scale_color_manual(
#'     values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
#'   )
atp_plot <- function(
    energetics,
    model = "ols",
    error_bar = "ci",
    conf_int = 0.95,
    size = 2,
    shape = 16,
    basal_vs_max = "basal",
    glyc_vs_resp = "glyc",
    group_label = "Experimental group",
    sep_reps = FALSE,
    ci_method = "Wald") {
  # Sanity checks
  stopifnot("'error_bar' should be 'sd' or 'ci'" = error_bar %in% c("sd", "ci"))
  stopifnot("'model' should be 'ols' or 'mixed'" = model %in% c("ols", "mixed"))
  stopifnot(
    "cannot run mixed-effects model with `sep_reps = TRUE`" =
      (model == "mixed" & !sep_reps) | (model == "ols")
  )
  stopifnot("'conf_int' should be between 0 and 1" = conf_int > 0 && conf_int < 1)

  data_cols <- c(
    "ATP_basal_glyc",
    "ATP_max_glyc",
    "ATP_basal_resp",
    "ATP_max_resp"
  )
  replicate <- NULL
  missing_cols <- setdiff(data_cols, colnames(energetics))
  if (length(missing_cols) != 0) {
    stop(paste0("'", missing_cols, "'", " column was not found in input data\n"))
  }

  # suppress "no visible binding for global variable" error
  exp_group <- NULL

  # TODO: make sep_reps = TRUE the default
  multi_rep <- length(unique(energetics$replicate)) > 1
  if (!sep_reps && missing(sep_reps) && multi_rep) warning(sep_reps_warning)

  energetics_summary <- get_energetics_summary(
    energetics,
    model = model,
    error_metric = error_bar,
    conf_int = conf_int,
    sep_reps = sep_reps
  )
  # Identify numeric columns and replace their negative values with 0
  numeric_cols <- names(energetics_summary)[sapply(energetics_summary, is.numeric)]
  energetics_summary[, (numeric_cols) := lapply(.SD, function(x) pmax(x, 0)), .SDcols = numeric_cols]

  # Determine which plot to create based on the options
  data_column <- switch(paste(basal_vs_max, glyc_vs_resp, sep = "_"),
    "basal_glyc" = "ATP_basal_glyc",
    "basal_resp" = "ATP_basal_resp",
    "max_glyc" = "ATP_max_glyc",
    "max_resp" = "ATP_max_resp",
    stop(
      "Invalid option: basal_vs_max must be either \"basal\" or \"max\" and glyc_vs_resp must be either \"glyc\" or \"resp\""
    )
  )

  # Set the data columns used for plotting
  data_column.mean <- paste0(data_column, ".mean")
  data_column.lower_bound <- paste0(data_column, ".lower_bound")
  data_column.higher_bound <- paste0(data_column, ".higher_bound")

  # Set the axis labels
  basal_vs_max_label <- ifelse(basal_vs_max == "basal", "Basal", "Max")
  glyc_vs_resp_label <- ifelse(glyc_vs_resp == "glyc", "Glycolysis", "Respiration")

  # The selected columns are plotted using data masks with the .data[[]] syntax
  # https://adv-r.hadley.nz/evaluation.html#data-masks
  ggplot(
    energetics_summary,
    aes(
      x = exp_group, y = .data[[data_column.mean]],
      color = if (sep_reps && multi_rep) replicate else NULL
    )
  ) +
    geom_point(
      size = size,
      shape = shape,
      position = position_dodge2(width = 0.3)
    ) +
    geom_linerange(
      aes(
        x = exp_group, y = .data[[data_column.mean]],
        ymin = .data[[data_column.lower_bound]],
        ymax = .data[[data_column.higher_bound]],
      ),
      data = energetics_summary,
      position = position_dodge2(width = 0.3)
    ) +
    xlab(group_label) +
    ylab("ATP Production (JATP)") +
    labs(title = paste0("ATP Production from ", basal_vs_max_label, " ", glyc_vs_resp_label)) +
    labs(color = "Replicate") +
    theme_bw()
}
