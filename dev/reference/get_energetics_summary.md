# Calculate ATP Production Mean and Standard Deviation

Calculates mean and standard deviation of ATP production from glycolysis
and OXPHOS at points defined in `partition_data` and with values
calculated using the `get_energetics` function via ordinary least
squares or a mixed-effects model

## Usage

``` r
get_energetics_summary(
  energetics,
  model = "ols",
  error_metric = "ci",
  conf_int = 0.95,
  sep_reps = FALSE,
  ci_method = "Wald"
)
```

## Arguments

- energetics:

  a data.table of Seahorse OCR and ECAR rates (from `get_energetics`)

- model:

  The model used to estimate mean and confidence intervals: ordinary
  least squares (`"ols"`) or mixed-effects (`"mixed"`)

- error_metric:

  Whether to calculate error as standard deviation (`"sd"`) or
  confidence intervals (`"ci"`)

- conf_int:

  The confidence interval percentage. Should be between 0 and 1

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

a list of groups from the data

## Details

To get the means and confidence intervals for experiments with
replicates, users can either use `sep_reps = TRUE` to get
replicate-level summary statistics or set `model = "mixed"` to use a
linear mixed-effects model on with replicate as the random-effect. The
confidence intervals are generated using `confint(method = "Wald")`.

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
energetics_list <- get_energetics(
  partitioned_data,
  ph = 7.4,
  pka = 6.093,
  buffer = 0.1
)
energetics_summary <- get_energetics_summary(energetics_list, sep_reps = FALSE)
head(energetics_summary[, c(1:5)], n = 10)
#> Key: <exp_group>
#>    exp_group count ATP_basal_resp.mean ATP_basal_resp.sd ATP_basal_resp.se
#>       <fctr> <int>               <num>             <num>             <num>
#> 1:   Group_1    22           1100.0468          72.25935         15.405746
#> 2:   Group_2    24           1136.0653          41.34070          8.438635
#> 3:   Group_3    24           1317.1044          52.53006         10.722653
#> 4:   Group_4    22            626.1267          85.60314         18.250651
head(energetics_summary[, c(1, 2, 6, 7)], n = 10)
#> Key: <exp_group>
#>    exp_group count ATP_basal_resp.lower_bound ATP_basal_resp.higher_bound
#>       <fctr> <int>                      <num>                       <num>
#> 1:   Group_1    22                  1069.8521                   1130.2415
#> 2:   Group_2    24                  1119.5259                   1152.6047
#> 3:   Group_3    24                  1296.0883                   1338.1204
#> 4:   Group_4    22                   590.3561                    661.8973
```
