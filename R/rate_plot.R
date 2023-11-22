#' Rate plot
#'
#' Generate OCR and ECAR plots
#' @param seahorse_rates data.table Seahorse OCR and ECAR rates (imported using `read_data` function)
#' @param measure Whether to plot `"OCR"` or `"ECAR"`
#' @param error_bar Whether to plot error bars as standard deviation (`"sd"`) or confidence intervals (`"ci"`)
#' @param conf_int The confidence interval percentage. Should be between 0 and 1
#' @param group_lab Label for the experimental group to populate the legend title
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon scale_x_continuous xlab ylab labs theme_bw
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' rate_plot(seahorse_rates, measure = "OCR", error_bar = "ci", conf_int = 0.95)
rate_plot <- function(
    seahorse_rates,
    measure = "OCR",
    error_bar = "ci",
    conf_int = 0.95,
    group_label = "Experimental group") {
  # sanity checks
  if (!measure %in% c("OCR", "ECAR")) {
    stop("'measure' should be 'OCR' or 'ECAR'")
  }

  if (!error_bar %in% c("sd", "ci")) {
    stop("'error_bar' should be 'sd' or 'ci'")
  }

  if (conf_int < 0 || conf_int > 1) {
    stop("'conf_int' should be between 0 and 1")
  }

  data_cols <- c(
    "Measurement",
    "Well",
    "OCR",
    "ECAR",
    "PER",
    "exp_group",
    "assay_type",
    "replicate"
  )
  missing_cols <- setdiff(data_cols, colnames(seahorse_rates))
  if (length(missing_cols) != 0) {
    stop(paste0("'", missing_cols, "'", " column was not found in input data\n"))
  }

  Measurement <- NULL
  exp_group <- NULL
  lower_bound <- NULL
  upper_bound <- NULL

  plot_data <- get_rate_summary(seahorse_rates, measure, error_bar, conf_int)

  y_labels <- list("OCR" = "OCR (pmol/min)", "ECAR" = "ECAR (mpH/min)")
  # plot function
  ggplot(plot_data, aes(
    x = Measurement,
    y = mean,
    color = exp_group,
    group = exp_group,
    fill = exp_group
  )) +
    geom_line(size = 2) +
    geom_ribbon(
      aes(
        ymin = lower_bound,
        ymax = upper_bound
      ),
      alpha = 0.2,
      color = NA
    ) +
    scale_x_continuous(breaks = seq(1, 12, by = 1)) +
    xlab("Measurement") +
    ylab(y_labels[measure]) +
    labs(color = group_label, fill = group_label) +
    theme_bw()
}

#' Rates summary
#'
#' Summarize OCR and ECAR as mean and bounded standard deviations or standard error with confidence intervals
#' @param seahorse_rates data.table Seahorse OCR and ECAR rates (imported using `read_data` function)
#' @param measure Whether to plot `"OCR"` or `"ECAR"`
#' @param error_metric Whether to calculate error as standard deviations (`"sd"`) or confidence intervals (`"ci"`)
#' @param conf_int The confidence interval percentage. Should be between 0 and 1
#' @return a data.table with means, standard deviations/standard error with bounds around the mean(sd or confidence intervals)
#'
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |>
#'   list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2)
#' rates <- get_rate_summary(
#'   seahorse_rates,
#'   measure = "OCR",
#'   error_metric = "ci",
#'   conf_int = 0.95
#' )
#' head(rates, n = 10)
get_rate_summary <- function(
    seahorse_rates,
    measure = "OCR",
    error_metric = "ci",
    conf_int = 0.95) {
  Measurement <- NULL
  exp_group <- NULL
  se <- NULL
  . <- NULL

  plot_data <- seahorse_rates[exp_group != "Background"][, .(
    mean = mean(get(measure)),
    sd = sd(get(measure)),
    se = sd(get(measure)) / sqrt(length(get(measure)))
  ), by = list(exp_group, Measurement)]

  z_value <- qnorm(((1 - conf_int) / 2), lower.tail = FALSE)

  if (error_metric == "sd") {
    plot_data[, `:=`(
      lower_bound = mean - sd,
      upper_bound = mean + sd
    )][]
  } else {
    plot_data[, `:=`(
      lower_bound = mean - (z_value * se),
      upper_bound = mean + (z_value * se)
    )][]
  }
}
