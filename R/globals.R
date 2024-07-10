# warning that the default `sep_reps = FALSE` will be changed in a future release
# found in get_energetics_summary, bioscope_plot, atp_plot, rate_plot
sep_reps_warning <- paste(
  "Replicates were combined within groups, but",
  "future releases of ceas will calculate energetics without combining them",
  "- use `sep_reps = FALSE` to maintain replicate combination."
)
