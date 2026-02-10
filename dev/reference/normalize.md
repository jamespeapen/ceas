# Normalize Seahorse data

Normalizes input data according to a sample normalization measure (e.g.
cell number or \\\mu\\g of protein). It assumes your data is background
normalized.

## Usage

``` r
normalize(
  seahorse_rates,
  norm_csv,
  norm_column = "well",
  norm_method = "minimum"
)
```

## Arguments

- seahorse_rates:

  The seahorse rates table read by the
  [`read_data()`](https://jamespeapen.github.io/ceas/dev/reference/read_data.md)
  function.

- norm_csv:

  A csv file with either well or experimental group in column 1 and the
  sample normalization measure in column 2. Headers are ignored.

- norm_column:

  Whether to normalize by `"well"` or `"exp_group"` group. The first
  column of the normalization csv provided should match this value.

- norm_method:

  How to normalize each well or experimental group (specified by
  `norm_column`):

  - by its corresponding row in the `norm_csv` (`"self"`) or

  - by the minimum of the `measure` column in the provided `norm_csv`
    (`"minimum"`).

  See details.

## Value

a normalized seahorse_rates data.table

## Details

This normalization is distinct from the background normalization done by
the Wave software. If the data are not background normalized,
[`read_data()`](https://jamespeapen.github.io/ceas/dev/reference/read_data.md)
will output a warning showing rows with OCR, ECAR and PER values greater
than 0.

### Normalization Methods

When `norm_method` is set to `"self"`, each OCR, ECAR, and PER value is
divided by the measure it"self". OCR and ECAR values are divided by the
corresponding raw value in the "measure" column: an intra-well or
experimental group normalization. Each normalized value is then
interpreted as pmol/min per measure (e.g. pmol/min/cell or
pmol/min/\\\mu\\g of protein.

When set to `"minimum"`, each OCR, ECAR, and PER value is normalized by
the minimum value in the `norm_csv` "measure" column. In this method,
every "measure" column's value in the provided CSV file is divided by
the lowest of the "measure" values to get a normalization factor for
each well or experimental group. The OCR, ECAR, and PER values in each
well or experimental group are divided by their corresponding
normalization factors. Compared to `"self"`, this is an
inter-well/experimental group normalization based on the lowest
`"measure"`. The results may be interpreted as pmol/min per minimum of
the measure (eg: group cell count or \\\mu\\g of protein.)

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
norm_csv <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "^norm.csv", full.names = TRUE)
read.csv(norm_csv)
#>   exp_group measure
#> 1   Group_1   30000
#> 2   Group_2   30000
#> 3   Group_3    5000
#> 4   Group_4    5000
seahorse_rates <- read_data(rep_list, sheet = 2)
head(seahorse_rates, n = 10)
#>     Measurement   Well     Time      OCR     ECAR      PER  exp_group
#>           <num> <char>    <num>    <num>    <num>    <num>     <char>
#>  1:           1    A01 1.304765   0.0000  0.00000   0.0000 Background
#>  2:           1    A02 1.304765 305.2426 30.64529 334.4771    Group_1
#>  3:           1    A03 1.304765 307.9862 33.27668 358.4754    Group_1
#>  4:           1    A04 1.304765 339.3399 49.17751 503.4910    Group_2
#>  5:           1    A05 1.304765 321.9398 47.94602 492.2597    Group_2
#>  6:           1    A06 1.304765 323.7962 46.84232 482.1940    Group_2
#>  7:           1    A07 1.304765 379.1455 46.81741 481.9668    Group_3
#>  8:           1    A08 1.304765 391.1478 50.14648 512.3280    Group_3
#>  9:           1    A09 1.304765 393.4523 52.54649 534.2160    Group_3
#> 10:           1    A10 1.304765 217.0543 29.11793 320.5476    Group_4
#>     assay_type replicate
#>         <char>    <fctr>
#>  1:       <NA>         1
#>  2:       MITO         1
#>  3:       MITO         1
#>  4:       MITO         1
#>  5:       MITO         1
#>  6:       MITO         1
#>  7:       MITO         1
#>  8:       MITO         1
#>  9:       MITO         1
#> 10:       MITO         1
# normalize by experimental group based on the minimum cell count or protein quantity
seahorse_rates.normalized <- normalize(
  seahorse_rates,
  norm_csv,
  norm_column = "exp_group",
  norm_method = "minimum"
)
head(seahorse_rates.normalized, n = 10)
#>     Measurement   Well     Time       OCR      ECAR       PER  exp_group
#>           <num> <char>    <num>     <num>     <num>     <num>     <char>
#>  1:           1    A01 1.304765   0.00000  0.000000   0.00000 Background
#>  2:           1    A02 1.304765  50.87376  5.107549  55.74619    Group_1
#>  3:           1    A03 1.304765  51.33103  5.546114  59.74590    Group_1
#>  4:           1    A04 1.304765  56.55665  8.196252  83.91516    Group_2
#>  5:           1    A05 1.304765  53.65663  7.991003  82.04329    Group_2
#>  6:           1    A06 1.304765  53.96603  7.807053  80.36566    Group_2
#>  7:           1    A07 1.304765 379.14553 46.817412 481.96685    Group_3
#>  8:           1    A08 1.304765 391.14776 50.146484 512.32798    Group_3
#>  9:           1    A09 1.304765 393.45230 52.546486 534.21600    Group_3
#> 10:           1    A10 1.304765 217.05432 29.117934 320.54760    Group_4
#>     assay_type replicate
#>         <char>    <fctr>
#>  1:       <NA>         1
#>  2:       MITO         1
#>  3:       MITO         1
#>  4:       MITO         1
#>  5:       MITO         1
#>  6:       MITO         1
#>  7:       MITO         1
#>  8:       MITO         1
#>  9:       MITO         1
#> 10:       MITO         1
```
