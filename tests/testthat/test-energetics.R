library(data.table)
get_energetics_input <- list(
  basal = data.table(OCR = c(200, 300, 400), ECAR = c(20, 35, 50), assay_type = rep("MITO", 3), exp_group = c("a", "b", "c"), replicate = rep(1, 3)),
  nonmito = data.table(OCR = c(30, 45, 60), ECAR = c(40, 55, 70), assay_type = rep("MITO"), exp_group = c("a", "b", "c"), replicate = rep(1, 3)),
  max_glyc = data.table(OCR = c(200, 300, 400), ECAR = c(50, 70, 90), assay_type = rep("GLYCO", 3), exp_group = c("a", "b", "c"), replicate = rep(1, 3)),
  uncoupled = data.table(OCR = c(50, 60, 70), ECAR = c(30, 40, 50), assay_type = rep("MITO", 3), exp_group = c("a", "b", "c"), replicate = rep(1, 3)),
  no_glucose_glyc = data.table(OCR = c(200, 300, 400), ECAR = c(10, 20, 30), assay_type = rep("GLYCO", 3), exp_group = c("a", "b", "c"), replicate = rep(1, 3)),
  maxresp = data.table(OCR = c(300, 400, 500), ECAR = c(30, 40, 50), assay_type = rep("GLYCO", 3), exp_group = c("a", "b", "c"), replicate = rep(1, 3)),
  glucose_glyc = data.table(OCR = c(200, 300, 400), ECAR = c(30, 40, 50), assay_type = rep("GLYCO", 3), exp_group = c("a", "b", "c"), replicate = rep(1, 3))
)

get_energetics_expected <- data.table::data.table(
  exp_group = factor(c("a", "b", "c")),
  replicate = as.factor(rep(1, 3)),
  ATP_basal_resp = c(862.5056, 1375.8950, 1889.2844),
  ATP_max_resp = c(886.7056, 1400.0950, 1913.4844),
  ATP_basal_glyc = c(194.7699, 242.1549, 289.5398),
  ATP_max_glyc = c(394.7699, 542.1549, 689.5398)
)

test_that("get_energetics returns the correct output", {
  result <- get_energetics(get_energetics_input, ph = 7.4, pka = 6.093, buffer = 0.1)
  expect_equal(result, get_energetics_expected, tolerance = 1e-7)
})
