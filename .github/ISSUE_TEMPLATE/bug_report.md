---
name: Bug report
about: Submit a bug found in ceas
title: "[BUG]: ..."
labels: bug
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
To investigate, we need a minimal reproducible example. We recommend using the datasets bundled with ceas:

```r
input_excel_files <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)

# code that produces the bug
# ...
```

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Desktop (please complete the following information):**
 - OS: [e.g. MacOS]
 - R version: [e.g. 4.3.3]
 - *ceas* version [e.g. 1.0.0 or GitHub SHA if you have a development version]

**Additional context**
Add any other context about the problem here.
