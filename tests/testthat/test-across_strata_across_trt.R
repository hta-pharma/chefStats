test_that("p-val interaction work (breslow-day)", {
  # SETUP -------------------------------------------------------------------
  ep <- ep_base(
    stratify_by = list(c("SEX")),
    data_prepare = mk_adae,
    custom_pop_filter = "TRT01A != 'Xanomeline Low Dose'",
    endpoint_filter = "AEDECOD == \"ERYTHEMA\"",
    stat_across_strata_across_trt = list(p_val_interaction)
  ) |>
    helper_pipeline_across_strata_across_trt()
  # ACT ---------------------------------------------------------------------

  actual <-
    chef::apply_stats(ep$ep,
      ep$analysis_data_container,
      type = "stat_by_strata_across_trt"
    ) |>
    tidyr::unnest(cols = stat_result) |>
    setDT()

  # EXPECT ------------------------------------------------------------------
  x <- mk_adae()
  x[, has_event := FALSE]
  x[AEDECOD == "ERYTHEMA", has_event := TRUE]
  x <- x |>
    setorderv("has_event", order = -1L) |>
    unique(by = "USUBJID")

  # Outcome, on treatment
  a <-
    x[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA" & SEX == "M"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA" & SEX == "M"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD)) & SEX == "M"] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD)) & SEX == "M"] |>
    NROW()

  two_by_two_m <-
    matrix(
      data = c(a, b, c_, d),
      nrow = 2,
      ncol = 2,
      byrow = T
    )


  # Outcome, on treatment
  a <-
    x[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA" & SEX == "F"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA" & SEX == "F"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD)) & SEX == "F"] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD)) & SEX == "F"] |>
    NROW()
  two_by_two_f <-
    matrix(
      data = c(a, b, c_, d),
      nrow = 2,
      ncol = 2,
      byrow = T
    )

  # Combine male and female 2x2 tables
  arr <- simplify2array(list(two_by_two_m, two_by_two_f))
  expected <- breslowdaytest(arr,
    odds_ratio = NA,
    correct = FALSE
  )

  expect_equal(actual$value, expected$p.value)
  expect_type(actual$value, "double")
})
