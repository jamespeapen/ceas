#' Bioenergetic Scope Plot
#'
#' Generate the Bioenergetic Scope Plot
#' @param energetics_summary A summary table of calculated means and standard
#' deviations of ATP production from glycolysis and OXPHOS at points defined in
#' patitioned_data and with values calculated using get_energetics_summary
#' function
#' @param bioscope_plot Creates a 2D plot visualizing the mean and standard
#' deviation basal and maximal ATP production from glycolysis and OXPHOS for
#' each experimental group
#' @param size Size of the points
#' @param shape Shape of the points
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlab ylab geom_linerange xlim ylim
#' @export
#'
#' @examples
#' rep_list <- system.file("extdata", package = "ceas") |> list.files(pattern = "*.xlsx", full.names=TRUE)
#' seahorse_rates <- read_data(rep_list, sheet=2) #reads in data
#' partitioned_data <- partition_data(seahorse_rates)
#' energetics_list <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
#' energetics_summary <- get_energetics_summary(energetics_list)
#' bioscope_plot(energetics_summary)
#'
#' # to change fill, the geom_point shape number should be between 15 and 25
#' bioscope_plot(energetics_summary, shape = 21) + # filled circle
#' ggplot2::scale_fill_manual(values = c("#e36500", "#b52356", "#3cb62d", "#328fe1"))
#'
#' # to change color use ggplot2::scale_color_manual
#' bioscope_plot(energetics_summary) +
#' ggplot2::scale_color_manual(values = c("#e36500", "#b52356", "#3cb62d", "#328fe1"))


bioscope_plot <- function(
  energetics_summary,
  size = 2,
  shape = 21
) {

  # sanity checks
  data_cols <- c(
    "glyc_no_drugs.mean",
    "glyc_no_drugs.sd",
    "glyc_max_glyc.mean",
    "glyc_max_glyc.sd",
    "ox_no_drugs.mean",
    "ox_no_drugs.sd",
    "ox_max_ox.mean",
    "ox_max_ox.sd"
  )
  missing_cols <- setdiff(data_cols, colnames(energetics_summary))
  if (length(missing_cols) != 0) {
    stop(paste0("'", missing_cols, "'", " column was not found in input data\n"))
  }

  max_axis <- round(max(
      max(energetics_summary$glyc_max_glyc.mean, energetics_summary$ox_max_ox.mean)
    ), -3)

  ggplot(energetics_summary, aes(
    glyc_max_glyc.mean,
    ox_max_ox.mean,
    color = cell_line,
    fill = cell_line
  )) +
  geom_point(shape = shape, size = size) +
  geom_point(
    data = energetics_summary,
    aes(x = glyc_no_drugs.mean, y = ox_no_drugs.mean, color = cell_line),
    size = size,
    shape = shape
  ) +
  xlab("ATP Production from Glycolysis (JATP)") +
  ylab("ATP Production from OXPHOS (JATP)") +
  labs(color = "Cell line", fill = "Cell line") +
  xlim(0, max_axis) + ylim(0, max_axis) +
  geom_linerange(aes(
    x = glyc_max_glyc.mean, y = ox_max_ox.mean,
     ymin = ox_max_ox.mean - ox_max_ox.sd,
     ymax = ox_max_ox.mean + ox_max_ox.sd
  ), data = energetics_summary) +
  geom_linerange(aes(
    x = glyc_max_glyc.mean, y = ox_max_ox.mean,
     xmin = glyc_max_glyc.mean - glyc_max_glyc.sd,
     xmax = glyc_max_glyc.mean + glyc_max_glyc.sd
  ), data = energetics_summary) +
  geom_linerange(aes(
    x = glyc_max_glyc.mean, y = ox_no_drugs.mean,
    xmin = glyc_no_drugs.mean - glyc_no_drugs.sd,
    xmax = glyc_no_drugs.mean + glyc_no_drugs.sd
  ), data = energetics_summary) +
  geom_linerange(aes(
    x = glyc_no_drugs.mean, y = ox_no_drugs.mean,
    ymin = ox_no_drugs.mean - ox_no_drugs.sd,
    ymax = ox_no_drugs.mean + ox_no_drugs.sd
  ), data = energetics_summary)
}
