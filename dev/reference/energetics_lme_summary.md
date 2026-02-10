# Get mean and confidence intervals from energetics mixed-effects models

Runs linear mixed-effects models on the ATP measure columns from
`get_energetics` with replicates as the random-effect. Estimates mean
and confidence intervals for ATP production from glycolysis and OXPHOS
at points defined in `partition_data`

## Usage

``` r
energetics_lme_summary(atp_col, energetics, conf_int, ci_method)
```

## Arguments

- atp_col:

  The column name of the ATP measure - one of "ATP_basal_resp",
  "ATP_max_resp", "ATP_basal_glyc", "ATP_max_glyc"

- energetics:

  a data.table of Seahorse OCR and ECAR rates (from `get_energetics`)

- conf_int:

  The confidence interval percentage. Should be between 0 and 1

- ci_method:

  The method used to compute confidence intervals for the mixed-effects
  model: `"Wald"`, `"profile"`, or `"boot"` passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).

## Value

a data.table with mean and the confidence interval bounds by
experimental group

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
# Only for one column. For the full energetics table run
# `get_energetics_summary` with `model = "mixed"`.
energetics_lme_summary(
  "ATP_max_resp",
  energetics,
  conf_int = 0.95,
  ci_method = "Wald"
)
#>    exp_group ATP_max_resp.mean ATP_max_resp.higher_bound
#>       <char>             <num>                     <num>
#> 1:   Group_1         1141.1589                 1169.8792
#> 2:   Group_2         1193.9108                 1230.4585
#> 3:   Group_3         1350.6716                 1387.2193
#> 4:   Group_4          620.7805                  658.1142
#>    ATP_max_resp.lower_bound
#>                       <num>
#> 1:                1112.4385
#> 2:                1157.3631
#> 3:                1314.1239
#> 4:                 583.4467
```
