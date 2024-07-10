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
