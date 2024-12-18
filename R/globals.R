# warning that the default `sep_reps = FALSE` will be changed in a future release
# found in get_energetics_summary, bioscope_plot, atp_plot, rate_plot
sep_reps_warning <- paste(
  "Replicates were combined within groups, but",
  "future releases of ceas will calculate energetics without combining them",
  "- use `sep_reps = FALSE` to maintain replicate combination."
)

# warning that the default `norm_column = "exp_group"` will be changed in a future release
# found in read_data, normalize
norm_column_warning <- paste(
  "Normalizing by experimental group, but",
  "future releases of ceas will normalize using the 'well' column by default",
  "- set `norm_column = 'exp_group'` to maintain normalization by experimental group"
)

# warning that the default `norm_method = "minimum"` will be changed in a future release
# found in read_data, normalize
norm_method_warning <- paste(
  "Using `norm_method = 'minimum' to normalize each row by the smallest measure of the input normalization data, but`",
  "future releases of ceas will normalize each `norm_column` by the corresponding row of the input data",
  "- set `norm_method` = 'minimum'` to maintain normalization by the minimum"
)
