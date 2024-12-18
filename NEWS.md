# ceas 1.3.0

- `normalize()` can now normalize by each well (`norm_column = "well"`) as an
  alternative to the existing normalization by experimental group (`norm_column
= "exp_group"`)

- `normalize()` has two normalization methods.

  - `norm_method = "self"`: based on the corresponding well or experimental
    group row of the `measure` column in the input normalization CSV.

    |exp_group | measure|
    |:---------|-------:|
    |Group_1   |   30000|
    |Group_2   |   30000|
    |Group_3   |    5000|
    |Group_4   |    5000|

    Given the input normalization data above, normalizing by experimental group
    will divide each of those experimental group rows of the seahorse table by the
    corresponding `measure` value of the experimental group in the input CSV.
    Similarly, if normalizing by well, each set of well rows is normalized by
    the corresponding `measure` value of the well - the input normalization CSV
    must have a column for `well` instead of `exp_group` for every well in the
    Seahorse data.

  - based on the minimum of the `measure` column of the input normalization data
    (`norm_method = "minimum"`) (same as [before](#ceas-030)). A normalization
  constant is calculated dividing each well or experimental group `measure` by
  the minimum `measure`.

    |exp_group | measure| norm_const|
    |:---------|-------:|----------:|
    |Group_1   |   30000|    6      |
    |Group_2   |   30000|    6      |
    |Group_3   |    5000|    1      |
    |Group_4   |    5000|    1      |

    If normalizing by experimental group, each row of the seahorse table is
    divide by the group's normalization constant. Similarly, if normalizing by
    well, each well row is divided by the well's normalization constant.

  **Note:** the current default is to normalize by experimental group and using
  the minimum (`norm_column = "exp_group", norm_method = "minimum"`) to maintain
  backwards compatibility, but future releases will normalize by well and using
  each corresponding row (`norm_column = "well", norm_method = "self"`).

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
