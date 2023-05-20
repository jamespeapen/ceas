#' Bioenergetic scope plot
#'
#' Create a Bioenergetic scope plot
#' @param energetics_summary A summary table of ATP values
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot geom_point labs xlab ylab geom_linerange xlim ylim
#' @importFrom ggprism theme_prism
#' @export
#'
#' @examples
#' seahorse_rates <- read_data(seahorse_rates)
bioscope_plot <- function(energetics_summary) {

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
  xlim(0, max_axis) + ylim(0, max_axis)
}
