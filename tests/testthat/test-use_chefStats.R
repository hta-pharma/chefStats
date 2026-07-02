test_that("use_chefStats errors when given function name that already exists", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "R"))
  withr::local_dir(tmp)
  dump("n_subj", file = "R/test.R")
  expect_error(use_chefStats(fn_name = "n_subj", fn_type = "stat_by_strata_by_trt"), regexp = "The function name")
})
