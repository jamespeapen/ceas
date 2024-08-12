# ceas 1.2.1

- `read_data()` throws an error if the "Group" column of the input data is only
  one word that cannot be separated with the `delimiter` provided by the user.

# ceas 1.2.0

- Linear mixed-effects models can now be used to get mean and confidence
  intervals for datasets with replicates. ATP production, OCR and ECAR are
  modelled as the response with experimental group as a fixed effect and replicate
  as a random effect. Mixed-effects modelling is supported by
  `get_energetics_summary()`, `get_rate_summary()`, `bioscope_plot()`,
  `atp_plot()`, and `rate_plot()`.

# ceas 1.1.2

- `get_energetics()` now warns about possible mismatches between the replicates
  in the MITO and GLYCO groups instead of stopping as datasets with different
  replicate counts can cause a mismatch that may not be erroneous.

# ceas 1.1.1

- `rate_plot()` now has a `linewidth` parameter to set the width of its
  `geom_line`s

# ceas 1.1.0

- Separating replicates is now supported for getting `get_energetics_summary()`,
  `bioscope_plot()`, `atp_plot()` and `rate_plot()` with `sep_reps = TRUE`. This will
  calculate summary statistics for each replicate within a group instead of
  combining them. `atp_plot()` now uses a linerange plot instead of a crossbar
  plot and color to distinguish between replicates instead of experimental
  groups. There is no color if there are no replicates or if they are combined.

  **Note:** the current default is to combine replicates (`sep_reps = FALSE`) to
  maintain backwards compatibility, but future releases will separate them by
  default. If `sep_reps` is not explicitly set to `FALSE`, the functions will
  warn the user about this future change in defaults.

# ceas 1.0.3

- Preserve the replicate column when returning energetics from `get_energetics()`

# ceas 1.0.2

- `read_data()` returns the `replicate` column as a factor instead of numeric

# ceas 1.0.1

- Replace `geom_line`'s deprecated `size` option with `linewidth` in `rate_plot`

# ceas 1.0.0

 - First CRAN release.

# ceas 0.3.3

 - Don't divide by 0 when normalizing if already 0; PER is now normalized with
   OCR and ECAR.

# ceas 0.3.2

 - Warn if data are not normalized by checking that the "Background" group's
   OCR, ECAR, and PER values are 0.

# ceas 0.3.1

 - Add assay type as argument to plot ECAR/OCR meaningfully as all assay types
   were previously being summarized instead of just one.

# ceas 0.3.0

 - Add `normalize()`, a cell count/protein mass normalization function.
   `read_data` now can take a csv file with cell counts or protein mass ($\mu$g)
   for each of the experimental groups to normalize the data.
   An example csv is provided below for a dataset with 4 experimental groups:

   |exp_group | measure|
   |:---------|-------:|
   |Group_1   |   30000|
   |Group_2   |   30000|
   |Group_3   |    5000|
   |Group_4   |    5000|

# ceas 0.2.0

 - Add delimiter between group and assay labels as argument in `read_data()` to
   support delimiters other than <Space>.

# ceas 0.1.1

- Fix the calculation of coupled mitochondrial respiration
  ([#4](https://github.com/jamespeapen/ceas/issues/4)). This correction was
  published in <https://doi.org/10.1074/jbc.AAC118.004855>.

# ceas 0.1.0

First release before initial submission for publication.
