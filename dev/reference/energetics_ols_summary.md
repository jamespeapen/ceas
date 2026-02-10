# Get ordinary least squares mean and confidence intervals from energetics

Helper function to calculate mean and standard deviation of ATP
production from glycolysis and OXPHOS at points defined in
`partition_data` and with values calculated using the `get_energetics`
function. Should only be called from `get_energetics_summary` as the
function itself only operaes on a vector without any of the grouping
that `get_energetics` does.

## Usage

``` r
energetics_ols_summary(atp_col, error_metric, conf_int)
```

## Arguments

- atp_col:

  The column name of the ATP measure - one of "ATP_basal_resp",
  "ATP_max_resp", "ATP_basal_glyc", "ATP_max_glyc"

- error_metric:

  Whether to calculate error as standard deviation (`"sd"`) or
  confidence intervals (`"ci"`)

- conf_int:

  The confidence interval percentage. Should be between 0 and 1

## Value

a data.table with mean and the confidence interval bounds by
experimental group

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
energetics <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
# Only for one row and across all groups and replicates.
# For the full correctly grouped energetics table run
# `get_energetics_summary` with `model = "ols"`.
energetics_ols_summary(energetics$ATP_max_resp, error_metric = "ci", conf_int = 0.95)
#> $mean
#> [1] 1085.137
#> 
#> $sd
#> [1] 280.1745
#> 
#> $se
#> [1] 29.21021
#> 
#> $lower_bound
#> [1] 1027.886
#> 
#> $higher_bound
#> [1] 1142.388
#> 
```
