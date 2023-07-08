#' Bioenergetic scope plot
#'
#' Create a Bioenergetic scope plot
#' @param energetics_summary A summary table of ATP values
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlab ylab geom_linerange xlim ylim
#' @export
#'
#' @examples
#' replicate_list <- c("rep1.xlsx", "rep2.xlsx", "rep3.xlsx")
#' seahorse_rates <- read_data(replicate_list) |>
#'                   partition_data() |>
#'                   get_energetics(ph, pka, buffer) |>
#'                   get_energetics_summary()
#' bioscope_plot(seahorse_rates)
#'
#' # to change fill, the geom_point shape number should be between 15 and 25
#' bioscope_plot(seahorse_rates, shape = 21) + # filled circle
#' ggplot2::scale_fill_manual(values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
#'
#' # to change color use ggplot2::scale_color_manual
#' bioscope_plot(seahorse_rates) +
#' ggplot2::scale_color_manual(values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
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
