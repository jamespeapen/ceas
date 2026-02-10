# Organize Seahorse Data

Organizes Seahorse OCR and ECAR rates based on defined time points (i.e.
the Measurement column) during the experiment. This time point can be
specified if you are modifying the Mito and Glyco Stress Test (i.e. from
3 measurements per cycle to X measurements)

## Usage

``` r
partition_data(
  seahorse_rates,
  assay_types = list(basal = "MITO", uncoupled = "MITO", maxresp = "MITO", nonmito =
    "MITO", no_glucose_glyc = "GLYCO", glucose_glyc = "GLYCO", max_glyc = "GLYCO"),
  basal_tp = 3,
  uncoupled_tp = 6,
  maxresp_tp = 8,
  nonmito_tp = 12,
  no_glucose_glyc_tp = 3,
  glucose_glyc_tp = 6,
  max_glyc_tp = 8
)
```

## Arguments

- seahorse_rates:

  A data.table of OCR and ECAR rates returned by `read_data`

- assay_types:

  A list that configures data partitioning based on the type of assay.
  See details.

- basal_tp:

  Basal respiration time point. Must be less than `uncoupled_tp`

- uncoupled_tp:

  ATP-coupled respiration time point. Must be less than `maxresp_tp`

- maxresp_tp:

  Maximal uncoupled respiration time point. Must be less than
  `nonmito_tp`

- nonmito_tp:

  Non-mitochondrial respiration time point. Must be larger than
  `maxresp_tp`

- no_glucose_glyc_tp:

  No glucose added acidification time point. Must be less than
  `glucose_glyc_tp`

- glucose_glyc_tp:

  Glucose-associated acidification time point. Must be less than
  `max_glyc_tp`

- max_glyc_tp:

  Maximal acidification time point. Must be less than `twodg_glyc_tp`

## Value

a list of named time points from each assay cycle

## Details

**Note:** When we use the term 'max' in the package documentation we
mean the maximal experimental OCR and ECAR values rather than absolute
biological maximums.

`partition_data` sets up the rates data for ATP calculations by the
`get_energetics` function. To do this, it takes a list `assay_types`
with the named values `basal`, `uncoupled`, `maxresp`, `nonmito`,
`no_glucose_glyc`, `glucose_glyc`, and `max_glyc`. In the default
setting, it is configured for an experiment with both Mito and Glyco
assays. However, partitioning can be configured for other experimental
conditions.

- Only MITO data:

    partitioned_data <- partition_data(
      seahorse_rates,
      assay_types = list(
        basal = "MITO",
        uncoupled = "MITO",
        maxresp = "MITO",
        nonmito = "MITO",
        no_glucose_glyc = NA,
        glucose_glyc = "MITO",
        max_glyc = NA
      ),
      basal_tp = 3,
      uncoupled_tp = 6,
      maxresp_tp = 8,
      nonmito_tp = 12,
      no_glucose_glyc_tp = NA,
      glucose_glyc_tp = 3,
      max_glyc_tp = NA
    )

Respiratory control ratio (RCR) and glycolytic capacity (GC) assay:

    partitioned_data <- partition_data(
      seahorse_rates,
      assay_types = list(
        basal = "RCR",
        uncoupled = "RCR",
        maxresp = "RCR,"
        nonmito = "RCR",
        no_glucose_glyc = NA,
        glucose_glyc = "GC",
        max_glyc = "GC"
      ),
      basal_tp = 3,
      uncoupled_tp = 6,
      maxresp_tp = 8,
      nonmito_tp = 12,
      no_glucose_glyc = NA,
      glucose_glyc_tp = 3,
      max_glyc_tp = 9
    )

- Data according to Mookerjee *et al.* 2017 *J Biol
  Chem*;*292*:7189-207.

    partitioned_data <- partition_data(
      seahorse_rates,
      assay_types = list(
        basal = "RefAssay",
        uncoupled = "RefAssay",
        maxresp = NA,
        nonmito = "RefAssay",
        no_glucose_glyc = "RefAssay",
        glucose_glyc = "RefAssay",
        max_glyc = NA
      ),
      basal_tp = 5,
      uncoupled_tp = 10,
      nonmito_tp = 12,
      maxresp = NA,
      no_glucose_glyc_tp = 1,
      glucose_glyc_tp = 5,
      max_glyc = NA
    )

Also see the vignette.

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
```
