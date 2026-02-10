# Estimate mean and confidence intervals for ATP measures using a mixed-effects model

Estimates mean and standard deviation of ATP production from glycolysis
and OXPHOS at points defined in `partition_data` and with values
calculated using the `get_energetics` function

## Usage

``` r
rates_lme_summary(measure, assay, rates, conf_int, ci_method)
```

## Arguments

- measure:

  Whether to plot `"OCR"` or `"ECAR"`

- assay:

  What assay to plot (e.g. "MITO" or "GLYCO")

- rates:

  a data.table of Seahorse OCR and ECAR rates (from `get_energetics`)

- conf_int:

  The confidence interval percentage. Should be between 0 and 1

- ci_method:

  The method used to compute confidence intervals for the mixed-effects
  model: `"Wald"`, `"profile"`, or `"boot"` passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).

## Value

a list of groups from the data

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
rates_lme_summary(
  measure = "OCR",
  assay = "MITO",
  rates = seahorse_rates,
  conf_int = 0.95,
  ci_method = "Wald"
)
#> boundary (singular) fit: see help('isSingular')
#> boundary (singular) fit: see help('isSingular')
#>     exp_group Measurement      mean lower_bound upper_bound
#>        <char>       <num>     <num>       <num>       <num>
#>  1:   Group_1           1 307.89166   291.96673   323.81660
#>  2:   Group_2           1 348.81974   339.21203   358.42746
#>  3:   Group_3           1 388.77691   379.16919   398.38462
#>  4:   Group_4           1 226.10023   216.28587   235.91458
#>  5:   Group_1           2 292.75576   276.83832   308.67320
#>  6:   Group_2           2 329.93782   321.71013   338.16550
#>  7:   Group_3           2 369.78727   361.55959   378.01496
#>  8:   Group_4           2 216.02105   207.61640   224.42569
#>  9:   Group_1           3 288.08473   274.17510   301.99437
#> 10:   Group_2           3 324.12258   316.38460   331.86056
#> 11:   Group_3           3 363.67394   355.93596   371.41192
#> 12:   Group_4           3 213.01895   205.11454   220.92336
#> 13:   Group_1           4 101.19365    91.72564   110.66166
#> 14:   Group_2           4 117.85782   113.52722   122.18843
#> 15:   Group_3           4 136.45736   132.12675   140.78796
#> 16:   Group_4           4  83.47057    79.04682    87.89431
#> 17:   Group_1           5  97.45195    88.44895   106.45496
#> 18:   Group_2           5 119.40534   114.82492   123.98576
#> 19:   Group_3           5 132.45771   127.87729   137.03813
#> 20:   Group_4           5  88.50989    83.83096    93.18882
#> 21:   Group_1           6  97.55136    87.45242   107.65030
#> 22:   Group_2           6 128.25149   122.51076   133.99222
#> 23:   Group_3           6 136.23777   130.49704   141.97850
#> 24:   Group_4           6 106.28409   100.41989   112.14829
#> 25:   Group_1           7 443.41553   418.06763   468.76342
#> 26:   Group_2           7 572.60816   540.62823   604.58810
#> 27:   Group_3           7 554.05711   522.07717   586.03704
#> 28:   Group_4           7 214.31752   181.64977   246.98528
#> 29:   Group_1           8 457.96943   438.71539   477.22347
#> 30:   Group_2           8 563.15354   536.49754   589.80955
#> 31:   Group_3           8 502.38164   475.72563   529.03765
#> 32:   Group_4           8 190.92705   163.69773   218.15637
#> 33:   Group_1           9 481.07496   461.12582   501.02411
#> 34:   Group_2           9 557.32512   529.70678   584.94346
#> 35:   Group_3           9 474.22872   446.61038   501.84706
#> 36:   Group_4           9 180.99782   152.78547   209.21017
#> 37:   Group_1          10  53.26419    47.68983    58.83855
#> 38:   Group_2          10  60.79879    57.54308    64.05449
#> 39:   Group_3          10  70.38167    67.12596    73.63738
#> 40:   Group_4          10  46.19680    42.87107    49.52253
#> 41:   Group_1          11  54.98763    50.31468    59.66057
#> 42:   Group_2          11  62.88026    60.26676    65.49376
#> 43:   Group_3          11  69.75906    67.14555    72.37256
#> 44:   Group_4          11  43.25444    40.58473    45.92416
#> 45:   Group_1          12  53.66469    49.25649    58.07289
#> 46:   Group_2          12  61.64285    59.07303    64.21268
#> 47:   Group_3          12  67.32687    64.75705    69.89670
#> 48:   Group_4          12  40.82449    38.19939    43.44959
#>     exp_group Measurement      mean lower_bound upper_bound
#>        <char>       <num>     <num>       <num>       <num>
```
