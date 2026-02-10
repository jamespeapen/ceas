# Rate plot

Generate OCR and ECAR plots

## Usage

``` r
rate_plot(
  seahorse_rates,
  measure = "OCR",
  assay = "MITO",
  model = "ols",
  error_bar = "ci",
  conf_int = 0.95,
  group_label = "Experimental group",
  linewidth = 2,
  sep_reps = FALSE,
  ci_method = "Wald"
)
```

## Arguments

- seahorse_rates:

  data.table Seahorse OCR and ECAR rates (imported using `read_data`
  function)

- measure:

  Whether to plot `"OCR"` or `"ECAR"`

- assay:

  What assay to plot (e.g. "MITO" or "GLYCO")

- model:

  The model used to estimate mean and confidence intervals:

- error_bar:

  Whether to plot error bars as standard deviation (`"sd"`) or
  confidence intervals (`"ci"`)

- conf_int:

  The confidence interval percentage. Should be between 0 and 1

- group_label:

  Label for the experimental group to populate the legend title

- linewidth:

  Width for the lines, passed to `geom_line()`

- sep_reps:

  Whether to calculate summary statistics on the groups with replicates
  combined. The current default `FALSE` combines replicates, but future
  releases will default to `TRUE` providing replicate-specific
  summaries.

- ci_method:

  The method used to compute confidence intervals for the mixed-effects
  model: `"Wald"`, `"profile"`, or `"boot"` passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).

## Value

a ggplot

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
rate_plot(
  seahorse_rates,
  measure = "OCR",
  error_bar = "ci",
  conf_int = 0.95,
  sep_reps = FALSE
)
#> Ignoring unknown labels:
#> • linetype : "Replicate"

rate_plot(
  seahorse_rates,
  measure = "OCR",
  error_bar = "ci",
  conf_int = 0.95,
  sep_reps = TRUE
)
```
