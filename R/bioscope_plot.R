#' Bioenergetic Scope Plot
#'
#' Generate the Bioenergetic Scope Plot
#' @param energetics A table of calculated glycolysis and OXPHOS rates.
#' Returned by `get_energetics`
#' @param error_bar Whether to plot error bars as standard deviation (`"sd"`)
#' or confidence intervals (`"ci"`)
#' @param conf_int The confidence interval percentage. Should be between 0 and 1
#' @param bioscope_plot Creates a 2D plot visualizing the mean and standard
#' deviation basal and maximal ATP production from glycolysis and OXPHOS for
#' each experimental group
#' @param size Size of the points
#' @param basal_shape Shape of the points for basal values
#' @param max_shape Shape of the points for max values
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlab ylab geom_linerange xlim ylim theme_bw scale_shape_manual
#' @export
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
#' bioscope_plot(energetics)
#'
#' # to change fill, the geom_point shape should be between 15 and 20.
#' # These shapes are filled without border and will correctly show on the legend.
#' bioscope_plot(energetics, size = 3, basal_shape = 2, max_shape = 17) + # empty and filled triangle
#'   ggplot2::scale_fill_manual(
#'     values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
#'   )
#'
#' # to change color, use ggplot2::scale_color_manual
#' bioscope_plot(energetics) +
#'   ggplot2::scale_color_manual(
#'     values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
#'   )
bioscope_plot <- function(
    energetics,
    error_bar = "ci",
    conf_int = 0.95,
    size = 2,
    basal_shape = 1,
    max_shape = 19) {
  # sanity checks

  data_cols <- c(
    "ATP_basal_glyc",
    "ATP_max_glyc",
    "ATP_basal_resp",
    "ATP_max_resp"
  )

  if (!error_bar %in% c("sd", "ci")) {
    stop("'error_bar' should be 'sd' or 'ci'")
  }

  if (conf_int < 0 || conf_int > 1) {
    stop("'conf_int' should be between 0 and 1")
  }

  missing_cols <- setdiff(data_cols, colnames(energetics))
  if (length(missing_cols) != 0) {
    stop(paste0("'", missing_cols, "'", " column was not found in input data\n"))
  }

  # suppress "no visible binding for global variable" error
  ATP_max_glyc.mean <- NULL
  ATP_max_glyc.lower_bound <- NULL
  ATP_max_glyc.higher_bound <- NULL

  ATP_basal_glyc.mean <- NULL
  ATP_basal_glyc.lower_bound <- NULL
  ATP_basal_glyc.higher_bound <- NULL

  ATP_max_resp.mean <- NULL
  ATP_max_resp.lower_bound <- NULL
  ATP_max_resp.higher_bound <- NULL

  ATP_basal_resp.mean <- NULL
  ATP_basal_resp.lower_bound <- NULL
  ATP_basal_resp.higher_bound <- NULL

  cell_line <- NULL

  energetics_summary <- get_energetics_summary(
    energetics,
    error_metric = error_bar,
    conf_int = conf_int
  )

  # Identify numeric columns
  numeric_cols <- names(energetics_summary)[sapply(energetics_summary, is.numeric)]
  # Replace negative values with 0 only in numeric columns
  energetics_summary[, (numeric_cols) := lapply(.SD, function(x) pmax(x, 0)), .SDcols = numeric_cols]

  max_axis <- max(energetics_summary$ATP_max_glyc.higher_bound, energetics_summary$ATP_max_resp.higher_bound)

  ggplot(energetics_summary, aes(
    ATP_max_glyc.mean,
    ATP_max_resp.mean,
    color = cell_line,
    fill = cell_line
  )) +
    geom_point(size = size, aes(shape = "Max")) +
    geom_point(
      data = energetics_summary,
      aes(x = ATP_basal_glyc.mean, y = ATP_basal_resp.mean, color = cell_line, shape = "Basal"),
      size = size
    ) +
    xlab("ATP Production from Glycolysis (JATP)") +
    ylab("ATP Production from OXPHOS (JATP)") +
    labs(color = "Cell line", fill = "Cell line") +
    xlim(0, max_axis) +
    ylim(0, max_axis) +
    geom_linerange(aes(
      x = ATP_max_glyc.mean, y = ATP_max_resp.mean,
      ymin = ATP_max_resp.lower_bound,
      ymax = ATP_max_resp.higher_bound
    ), data = energetics_summary) +
    geom_linerange(aes(
      x = ATP_max_glyc.mean, y = ATP_max_resp.mean,
      xmin = ATP_max_glyc.lower_bound,
      xmax = ATP_max_glyc.higher_bound
    ), data = energetics_summary) +
    geom_linerange(aes(
      x = ATP_basal_glyc.mean, y = ATP_basal_resp.mean,
      xmin = ATP_basal_glyc.lower_bound,
      xmax = ATP_basal_glyc.higher_bound
    ), data = energetics_summary) +
    geom_linerange(aes(
      x = ATP_basal_glyc.mean, y = ATP_basal_resp.mean,
      ymin = ATP_basal_resp.lower_bound,
      ymax = ATP_basal_resp.higher_bound
    ), data = energetics_summary) +
    scale_shape_manual(name = "Value", values = c("Basal" = basal_shape, "Max" = max_shape)) +
    theme_bw()
}
