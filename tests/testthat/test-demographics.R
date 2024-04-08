test_that("Demographics (categorical) work when strata is provided", {

  # SETUP -------------------------------------------------------------------
  input <- mk_advs()
  input[, INDEX_ := .I] |> setkey(INDEX_)
  cell_index_f <-
    input[SAFFL == "Y" &
            TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]

  cell_index_total <-
    input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]


  # ACT ---------------------------------------------------------------------
  actual_total <- demographics_counts(
    input,
    cell_index = cell_index_total,
    subjectid_var = "USUBJID",
    stratify_by = c("TOTAL_", "AGEGR1", "SEX"),
    strata_var = "TOTAL_"
  )

  actual_f <- demographics_counts(
    input,
    cell_index = cell_index_f,
    subjectid_var = "USUBJID",
    stratify_by = c("TOTAL_", "AGEGR1", "SEX"),
    strata_var = "SEX"
  )

  # EXPECT ------------------------------------------------------------------

  x <- mk_advs()

  x <- x |> unique(by = "USUBJID")
  x[, missing_sex := FALSE]
  x[is.na(SEX), missing_sex := TRUE]
  b <-
    x[TRT01A == "Placebo", .N, by = .(missing_sex, SEX)] |> setorder(SEX)

  expect_equal(actual_total[qualifiers == "SEX" & label == "n_missing", value],
               b[(missing_sex), N])

  expect_snapshot_value(as.data.frame(actual_total),
                        tolerance = 1e-6, style = "json2")

  expect_snapshot_value(as.data.frame(actual_f),
                        tolerance = 1e-6, style = "json2")

})


test_that("Demographics (continuous) work when no strata level is provided", {
  # SETUP -------------------------------------------------------------------
  ep <- chef::mk_endpoint_str(
    data_prepare = mk_advs,
    treatment_var = "TRT01A",
    treatment_refval = "Xanomeline High Dose",
    pop_var = "SAFFL",
    pop_value = "Y",
    stat_by_strata_by_trt = list(c(demographics_continuous,
                                   var = "AGE")),
    endpoint_label = "Demographics endpoint (categorical measures)"
  ) |>
    helper_pipeline()

  # ACT ---------------------------------------------------------------------

  actual <-
    chef::apply_stats(ep$ep,
                      ep$analysis_data_container,
                      type = "stat_by_strata_by_trt") |>
    tidyr::unnest(cols = stat_result) |>
    setDT()

  # EXPECT ------------------------------------------------------------------
  x <- mk_advs()
  x <- x |> unique(by = "USUBJID")
  mean_ <- x[, mean(AGE), by = TRT01A] |> setorder(TRT01A)
  median_ <- x[, median(AGE), by = TRT01A] |> setorder(TRT01A)
  sd_ <- x[, sd(AGE), by = TRT01A] |> setorder(TRT01A)
  min_ <- x[, min(AGE), by = TRT01A] |> setorder(TRT01A)
  max_ <- x[, max(AGE), by = TRT01A] |> setorder(TRT01A)

  expect_equal(actual[grepl("mean", label), value], mean_[["V1"]],
               ignore_attr =
                 TRUE)
  expect_equal(actual[grepl("median", label), value], median_[["V1"]],
               ignore_attr =
                 TRUE)
  expect_equal(actual[grepl("sd", label), value], sd_[["V1"]],
               ignore_attr =
                 TRUE)
  expect_equal(actual[grepl("min", label), value], min_[["V1"]],
               ignore_attr =
                 TRUE)
  expect_equal(actual[grepl("max", label), value], max_[["V1"]],
               ignore_attr =
                 TRUE)
})
