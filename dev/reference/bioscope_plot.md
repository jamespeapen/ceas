# Bioenergetic Scope Plot

Generate the Bioenergetic Scope Plot

## Usage

``` r
bioscope_plot(
  energetics,
  model = "ols",
  error_bar = "ci",
  conf_int = 0.95,
  size = 2,
  basal_shape = 1,
  max_shape = 19,
  group_label = "Experimental Group",
  sep_reps = FALSE,
  ci_method = "Wald"
)
```

## Arguments

- energetics:

  A table of calculated glycolysis and OXPHOS rates. Returned by
  `get_energetics`

- model:

  The linear model used to estimate mean and confidence intervals:
  ordinary least squares (`"ols"`) or mixed-effects (`"mixed"`)

- error_bar:

  Whether to plot error bars as standard deviation (`"sd"`) or
  confidence intervals (`"ci"`)

- conf_int:

  The confidence interval percentage. Should be between 0 and 1

- size:

  Size of the points

- basal_shape:

  Shape of the points for basal values

- max_shape:

  Shape of the points for max values

- group_label:

  Label for the experimental group to populate the legend title

- sep_reps:

  Whether to calculate summary statistics on the groups with replicates
  combined. The current default `FALSE` combines replicates, but future
  releases will default to `TRUE` providing replicate-specific
  summaries.

- ci_method:

  The method used to compute confidence intervals for the mixed-effects
  model: `"Wald"`, `"profile"`, or `"boot"` passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).

- bioscope_plot:

  Creates a 2D plot visualizing the mean and standard deviation basal
  and maximal ATP production from glycolysis and OXPHOS for each
  experimental group

## Value

a ggplot

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
energetics <- get_energetics(
  partitioned_data,
  ph = 7.4,
  pka = 6.093,
  buffer = 0.1
)
bioscope_plot(energetics, sep_reps = FALSE)
#> Ignoring unknown labels:
#> • linetype : "Replicate"


# to change fill, the geom_point shape should be between 15 and 20.
# These shapes are filled without border and will correctly show on the legend.
bioscope_plot(
  energetics,
  sep_reps = TRUE,
  size = 3,
  basal_shape = 2,
  max_shape = 17 # empty and filled triangle
) +
  ggplot2::scale_fill_manual(
    values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
  )
#> Ignoring unknown labels:
#> • linetype : "Replicate"


# to change color, use ggplot2::scale_color_manual
bioscope_plot(energetics, sep_reps = FALSE) +
  ggplot2::scale_color_manual(
    values = c("#e36500", "#b52356", "#3cb62d", "#328fe1")
  )
#> Ignoring unknown labels:
#> • linetype : "Replicate"
```
