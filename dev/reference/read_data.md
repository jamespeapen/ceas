# Read Seahorse Wave Excel File

Reads input seahore data from an excel Seahorse Wave File. It assumes
your data is background normalized.

## Usage

``` r
read_data(
  rep_list,
  norm = NULL,
  sheet = 2,
  delimiter = " ",
  norm_column = "exp_group",
  norm_method = "minimum"
)
```

## Arguments

- rep_list:

  A list of Seahorse Wave excel export files. One file per replicate. If
  your data is in a directory called "seahorse_data", use
  `list.files("seahorse_data", pattern = "*.xlsx", full.names = TRUE)`
  to make a list of the excel files. Add multiple replicates with care -
  see details.

- norm:

  A csv file with the experimental groups and their normalization
  values. Leave unset if normalization is not required. See
  [`normalize()`](https://jamespeapen.github.io/ceas/dev/reference/normalize.md).

- sheet:

  The number of the excel sheet containing the long-form Seahorse data.
  Default is 2 because the long-form output from Seahorse Wave is on
  sheet 2

- delimiter:

  The delimiter between the group name and the assay type in the Group
  column of the wave output. e.g. "Group1 MITO" would use a space
  character as delimiter.

- norm_column:

  Whether to normalize by `"Well"` or `"exp_group"` column. The first
  column of the normalization csv provided should match this value.

- norm_method:

  How to normalize each well or experimental group (specified by
  `norm_column`):

  - by its corresponding row in the `norm` csv (`"self"`) or

  - by the minimum of the `measure` column in the provided `norm` csv
    (`"minimum"`).

  See the
  [`normalize()`](https://jamespeapen.github.io/ceas/dev/reference/normalize.md)
  function for more details.

## Value

a seahorse_rates table

## Details

Although ceas enables integration of multiple biological and/or
technical replicates, previous work has reported high inter-plate
variation (Yepez et. al 2018). If you don't want your replicate data
combined, you can either:

- make sure that the names of the common groups between the replicates
  are different.

- in downstream analyses (`get_energetics_summary`, `bioscope_plot`,
  `rate_plot`, `atp_plot`), use `sep_reps = TRUE` to do all calculations
  and plotting separately for each replicate.

**NOTE:** to maintain backwards compatibility `sep_reps` is currently
`FALSE` by default, but will be set to `TRUE` in a future release.

## References

Yépez *et al.* 2018 *OCR-Stats: Robust estimation and statistical
testing of mitochondrial respiration activities using Seahorse XF
Analyzer* *PLOS ONE* 2018;**13**:e0199938.
[doi:10.1371/journal.pone.0199938](https://doi.org/10.1371/journal.pone.0199938)

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
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

# normalization by well using raw cell count or protein quantity
norm_csv <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "well_norm.csv", full.names = TRUE)
seahorse_rates.norm <- read_data(
  rep_list,
  norm = norm_csv,
  norm_column = "well",
  norm_method = "self",
  sheet = 2
)
head(seahorse_rates.norm, n = 10)
#>     Measurement   Well     Time        OCR        ECAR        PER  exp_group
#>           <num> <char>    <num>      <num>       <num>      <num>     <char>
#>  1:           1    A01 1.304765 0.00000000 0.000000000 0.00000000 Background
#>  2:           1    A02 1.304765 0.06104851 0.006129059 0.06689542    Group_1
#>  3:           1    A03 1.304765 0.05599749 0.006050306 0.06517735    Group_1
#>  4:           1    A04 1.304765 0.06402640 0.009278776 0.09499830    Group_2
#>  5:           1    A05 1.304765 0.07154218 0.010654670 0.10939105    Group_2
#>  6:           1    A06 1.304765 0.05488070 0.007939376 0.08172779    Group_2
#>  7:           1    A07 1.304765 0.08425456 0.010403869 0.10710374    Group_3
#>  8:           1    A08 1.304765 0.06984781 0.008954729 0.09148714    Group_3
#>  9:           1    A09 1.304765 0.06668683 0.008906184 0.09054508    Group_3
#> 10:           1    A10 1.304765 0.04095365 0.005493950 0.06048068    Group_4
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
