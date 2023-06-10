#' Bioenergetic scope plot
#'
#' Create a Bioenergetic scope plot
#' @param energetics_summary A summary table of ATP values
#' @return a ggplot
#'
#' @export
#'
#' @examples
#' seahorse_rates <- read_data(seahorse_rates)
bioscope_plot <- function(energetics_summary) {

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

  ggplot(energetics_summary, aes(glyc_max_glyc.mean, ox_max_ox.mean, color = cell_line)) +
  geom_point() +
  geom_point(
    data = energetics_summary,
    aes(x = glyc_no_drugs.mean, y = ox_no_drugs.mean, color = cell_line)
  ) +
  xlab("ATP Production from Glycolysis (JATP)") +
  ylab("ATP Production from OXPHOS (JATP)") +
  scale_color_discrete(name = "Cell Line") +
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
