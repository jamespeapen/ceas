# Rates summary

Summarize OCR and ECAR as mean and bounded standard deviations or
standard error with confidence intervals

## Usage

``` r
get_rate_summary(
  seahorse_rates,
  measure = "OCR",
  assay,
  model = "ols",
  error_metric = "ci",
  conf_int = 0.95,
  sep_reps = FALSE,
  ci_method = "Wald"
)
```

## Arguments

- seahorse_rates:

  data.table Seahorse OCR and ECAR rates (imported using `read_data`
  function)

- measure:

  Whether to calculate summary for `"OCR"` or `"ECAR"`

- assay:

  What assay to calculate summary for (e.g. "MITO" or "GLYCO")

- model:

  The model used to estimate mean and confidence intervals:

- error_metric:

  Whether to calculate error as standard deviations (`"sd"`) or
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

a data.table with means, standard deviations/standard error with bounds
around the mean(sd or confidence intervals)

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
combined_reps <- get_rate_summary(
  seahorse_rates,
  measure = "OCR",
  assay = "MITO",
  model = "ols",
  error_metric = "ci",
  conf_int = 0.95,
  sep_reps = FALSE
)
head(combined_reps, n = 10)
#>     exp_group Measurement     mean       sd       se lower_bound upper_bound
#>        <char>       <num>    <num>    <num>    <num>       <num>       <num>
#>  1:   Group_1           1 307.8917 20.68089 4.409180    299.2498    316.5335
#>  2:   Group_2           1 348.8197 16.65077 3.398824    342.1582    355.4813
#>  3:   Group_3           1 388.7769 19.33691 3.947130    381.0407    396.5131
#>  4:   Group_4           1 226.1002 15.78665 3.365726    219.5035    232.6969
#>  5:   Group_1           2 292.7558 19.41210 4.138674    284.6441    300.8674
#>  6:   Group_2           2 329.9378 12.87193 2.627473    324.7881    335.0876
#>  7:   Group_3           2 369.7873 17.35845 3.543279    362.8426    376.7320
#>  8:   Group_4           2 216.0210 14.42133 3.074639    209.9949    222.0472
#>  9:   Group_1           3 288.0847 18.63144 3.972235    280.2993    295.8702
#> 10:   Group_2           3 324.1226 11.68031 2.384234    319.4496    328.7956

# separate replicates
sep_reps <- get_rate_summary(
  seahorse_rates,
  measure = "OCR",
  assay = "MITO",
  model = "ols",
  error_metric = "ci",
  conf_int = 0.95,
  sep_reps = TRUE
)
head(sep_reps, n = 10)
#>     exp_group Measurement replicate     mean       sd       se lower_bound
#>        <char>       <num>    <fctr>    <num>    <num>    <num>       <num>
#>  1:   Group_1           1         1 317.2648 21.57755 6.505875    304.5136
#>  2:   Group_2           1         1 349.2271 17.16613 4.955436    339.5147
#>  3:   Group_3           1         1 399.4677 14.30962 4.130832    391.3714
#>  4:   Group_4           1         1 236.0472 13.21393 3.984150    228.2384
#>  5:   Group_1           2         1 302.3107 20.03568 6.040984    290.4705
#>  6:   Group_2           2         1 330.4760 13.43898 3.879499    322.8723
#>  7:   Group_3           2         1 381.0119 12.00461 3.465432    374.2198
#>  8:   Group_4           2         1 225.7442 11.59172 3.495034    218.8940
#>  9:   Group_1           3         1 296.7702 19.66343 5.928748    285.1501
#> 10:   Group_2           3         1 323.9365 12.40370 3.580640    316.9185
#>     upper_bound
#>           <num>
#>  1:    330.0161
#>  2:    358.9396
#>  3:    407.5640
#>  4:    243.8560
#>  5:    314.1508
#>  6:    338.0797
#>  7:    387.8041
#>  8:    232.5943
#>  9:    308.3903
#> 10:    330.9544

# mixed effects model
reps_as_random_effects <- get_rate_summary(
  seahorse_rates,
  measure = "OCR",
  assay = "MITO",
  model = "mixed",
  error_metric = "ci",
  conf_int = 0.95,
  sep_reps = FALSE
)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
head(reps_as_random_effects, n = 10)
#>     exp_group Measurement     mean lower_bound upper_bound
#>        <char>       <num>    <num>       <num>       <num>
#>  1:   Group_1           1 307.8917    291.9667    323.8166
#>  2:   Group_2           1 348.8197    339.2120    358.4275
#>  3:   Group_3           1 388.7769    379.1692    398.3846
#>  4:   Group_4           1 226.1002    216.2859    235.9146
#>  5:   Group_1           2 292.7558    276.8383    308.6732
#>  6:   Group_2           2 329.9378    321.7101    338.1655
#>  7:   Group_3           2 369.7873    361.5596    378.0150
#>  8:   Group_4           2 216.0210    207.6164    224.4257
#>  9:   Group_1           3 288.0847    274.1751    301.9944
#> 10:   Group_2           3 324.1226    316.3846    331.8606
```
