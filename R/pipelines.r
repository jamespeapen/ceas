#' Bioenergetic scope plot pipeline
#'
#' Create a Bioenergetic scope plot from input seahorse rate Excel files
#' @param rep_list A list of paths to the excel files. One file per replicate
#' @param ph pH value for energetics calculation
#' @param pka pKa value for energetics calculation
#' @param buffer buffer for energetics calculation
#' @return a ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_point labs xlab ylab geom_linerange xlim ylim scale_color_discrete
#' @export
#'
#' @examples
#' replicate_list <- c("rep1.xlsx", "rep2.xlsx", "rep3.xlsx")
#' make_bioscope_plot(rep_list, ph, pka, buffer)
make_bioscope_plot <- function(rep_list, ph, pka, buffer) {
  read_seahorse(rep_list) |>
    partition_data() |>
    get_energetics(ph = ph, pka = pka, buffer = buffer) |>
    get_energetics_summary() |>
    bioscope_plot()
}
