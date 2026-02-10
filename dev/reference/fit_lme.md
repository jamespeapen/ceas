# Estimate mean and confidence intervals using a mixed-effects model

Estimates mean and standard deviation of energetics or rates with
replicates as the random-effect

## Usage

``` r
fit_lme(
  data_col,
  input,
  group_colname = "exp_group",
  rep_colname = "replicate"
)
```

## Arguments

- data_col:

  The column name of the ATP measure ("ATP_basal_resp", "ATP_max_resp",
  "ATP_basal_glyc", "ATP_max_glyc") or rate measure ("OCR", "ECAR")

- input:

  The dataset containing `data_col` from `get_energetics` or `read_data`

- group_colname:

  The column containing experimental group names

- rep_colname:

  The column containing replicate IDs

## Value

an [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) mixed effects
model

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
energetics <- get_energetics(partitioned_data, ph = 7.4, pka = 6.093, buffer = 0.1)
fit_lme("ATP_max_glyc", energetics)
#> boundary (singular) fit: see help('isSingular')
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: ATP_max_glyc ~ exp_group + (1 | replicate)
#>    Data: input
#> REML criterion at convergence: 1014.011
#> Random effects:
#>  Groups    Name        Std.Dev.
#>  replicate (Intercept)  0.00   
#>  Residual              71.61   
#> Number of obs: 92, groups:  replicate, 2
#> Fixed Effects:
#>      (Intercept)  exp_groupGroup_2  exp_groupGroup_3  exp_groupGroup_4  
#>            470.7             462.9             371.2             149.0  
#> optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 
```
