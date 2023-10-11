#' ATP Plot
#'
#' Generate the ATP Plot
#' @param energetics A table of calculated glycolysis and OXPHOS rates.
#' Returned by `get_energetics`
#' @param error_bar Whether to plot error bars as standard deviation (`"sd"`)
#' or confidence intervals (`"ci"`)
#' @param conf_int The confidence interval percentage. Should be between 0 and 1
#' @param size Size of the points
#' @param shape Shape of the points
#' @param basal_vs_max Whether to plot `"basal"` or `"max"` respiration
#' @param glyc_vs_resp Whether to plot glycolysis (`"glyc"`)  or respiration (`"resp"`)
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlab ylab geom_linerange xlim ylim geom_crossbar theme_bw .data
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |> list.files(pattern = "*.xlsx", full.names = TRUE)
#' seahorse_rates <- read_data(rep_list, sheet = 2) # reads in data
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
#' atp_plot(energetics)
#'
#' atp_plot(energetics, basal_vs_max = "max")
#'
#' atp_plot(energetics, basal_vs_max = "basal", glyc_vs_resp = "resp")
#'
#' # to change fill, the geom_point shape number should be between 15 and 25
#' atp_plot(energetics, shape = 21) + # filled circle
#'   ggplot2::scale_fill_manual(values = c("#e36500", "#b52356", "#3cb62d", "#328fe1"))
#'
#' # to change color, use ggplot2::scale_color_manual
#' atp_plot(energetics) +
#'   ggplot2::scale_color_manual(values = c("#e36500", "#b52356", "#3cb62d", "#328fe1"))
atp_plot <- function(
    energetics,
    error_bar = "ci",
    conf_int = 0.95,
    size = 2,
    shape = 21,
    basal_vs_max = "basal",
    glyc_vs_resp = "glyc"
    ) {

  # Sanity checks
  if (!error_bar %in% c("sd", "ci")) {
    stop("'error_bar' should be 'sd' or 'ci'")
  }

  if (conf_int < 0 || conf_int > 1) {
    stop("'conf_int' should be between 0 and 1")
  }

  data_cols <- c(
    "ATP_basal_glyc",
    "ATP_max_glyc",
    "ATP_basal_resp",
    "ATP_max_resp"
  )

  missing_cols <- setdiff(data_cols, colnames(energetics))
  if (length(missing_cols) != 0) {
    stop(paste0("'", missing_cols, "'", " column was not found in input data\n"))
  }

  # suppress "no visible binding for global variable" error
  cell_line <- NULL

  energetics_summary <- get_energetics_summary(
    energetics,
    error_metric = error_bar,
    conf_int = conf_int
  )
  # Identify numeric columns and replace their negative values with 0
  numeric_cols <- names(energetics_summary)[sapply(energetics_summary, is.numeric)]
  energetics_summary[, (numeric_cols) := lapply(.SD, function(x) pmax(x, 0)), .SDcols = numeric_cols]

  # Determine which plot to create based on the options
  data_column <- switch(
    paste(basal_vs_max, glyc_vs_resp, sep = "_"),
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
      x = cell_line, y = .data[[data_column.mean]],
      cell_line,
      color = cell_line,
      fill = cell_line
    )
  ) +
    geom_point(size = size, shape = shape) +
    geom_crossbar(
      aes(
        x = cell_line, y = .data[[data_column.mean]],
        ymin = .data[[data_column.lower_bound]],
        ymax = .data[[data_column.higher_bound]],
      ),
      alpha = 0.5,
      data = energetics_summary
    ) +
    xlab("Experimental Group") +
    ylab("ATP Production (JATP)") +
    labs(title = paste0("ATP Production from ", basal_vs_max_label, " ", glyc_vs_resp_label)) +
    labs(color = "Cell line", fill = "Cell line") +
    theme_bw()
}
