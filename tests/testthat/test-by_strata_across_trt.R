test_that("RR works", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  # ACT ---------------------------------------------------------------------
  AEDECOD <- TRT01A <- SAFFL <- USUBJID <- label <- NULL
  actual_total <- RR(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
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
      SAFFL == "Y" & AEDECOD == "ERYTHEMA"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" & AEDECOD == "ERYTHEMA"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  rr <- (a / sum(a, b)) / (c_ / sum(c_, d))
  expect_equal(actual_total[label == "RR"]$value, rr)
  expect_type(actual_total$value, "double")
})


test_that("RR works when 0 events", {
  # SETUP -------------------------------------------------------------------
  AESEV <- TRT01A <- SAFFL <- USUBJID <- label <-  AESOC <- SEX <- NULL
  input <- mk_adae(study_metadata = NULL)
  input[, INDEX_ := .I] |> data.table::setkey(INDEX_)
  cell_index_f <-
    input[SAFFL == "Y" &
            (TRT01A == "Placebo" |
               TRT01A == "Xanomeline High Dose")
             & SEX == "F"][["INDEX_"]]
  cell_index_total <-
    input[SAFFL == "Y" &
            (TRT01A == "Placebo" |
               TRT01A == "Xanomeline High Dose")][["INDEX_"]]
  event_index <-
    input[AESEV == "SEVERE" & AESOC == "GASTROINTESTINAL DISORDERS"][["INDEX_"]]
  
  
  # ACT ---------------------------------------------------------------------
  actual_female <- RR(
    dat = input,
    event_index = event_index,
    cell_index = cell_index_f,
    treatment_var = "TRT01A",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  
  # EXPECT ------------------------------------------------------------------
  x <- mk_adae(study_metadata = NULL)[TRT01A != "Xanomeline Low Dose"]
  x <- x[SAFFL == "Y" & SEX == "F"]
  x[, event := FALSE]
  x[AESEV == "SEVERE" &
    AESOC == "GASTROINTESTINAL DISORDERS", event := TRUE]
  x <- x |>
    setorderv("event", order = -1L) |>
    unique(by = "USUBJID")

  two_by_two_ish <- x[, .N, by = .(TRT01A, event)]
  # Outcome, on treatment
  a <-
    x[(event) &
      TRT01A == "Xanomeline High Dose"] |> NROW() + 0.5

  # NO Outcome, on treatment
  b <-
    x[!(event) &
      TRT01A == "Xanomeline High Dose"] |> NROW() + 0.5

  # Outcome, no treatment
  c <-
    x[(event) & TRT01A == "Placebo"] |> NROW() + 0.5

  # Outcome, no treatment
  d <-
    x[!(event) & TRT01A == "Placebo"] |> NROW() + 0.5

  rr <- (a / sum(a, b)) / (c / sum(c, d))
  se <-
    sqrt(+1 / a + 1 / c - 1 / sum(a,b) - 1 /
           sum(c,d))
  
  expect_equal(actual_female[label == "RR"]$value, rr)
  expect_equal(actual_female[label == "SE"]$value, se)
  
  
})


test_that("OR works", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  # ACT ---------------------------------------------------------------------
  actual <-
    OR(
      dat = setup$input,
      event_index = setup$event_index,
      cell_index = setup$cell_index_total,
      treatment_var = "TRT01A",
      treatment_refval = "Xanomeline High Dose",
      subjectid_var = "USUBJID"
    )
  
  # EXPECT ------------------------------------------------------------------
  x <- mk_adae(study_metadata = NULL)
  x[, has_event := FALSE]
  x[AEDECOD == "ERYTHEMA", has_event := TRUE]
  x <- x |>
    setorderv("has_event", order = -1L) |>
    unique(by = "USUBJID")

  # Outcome, on treatment
  a <-
    x[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" & AEDECOD == "ERYTHEMA"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" & AEDECOD == "ERYTHEMA"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  or <- prod(a, d) / prod(b, c_)
  expect_equal(actual[label == "OR"]$value, or)
  expect_type(actual$value, "double")
})


test_that("RD works", {
  # SETUP -------------------------------------------------------------------
  ep <- ep_base(
    stratify_by = list(c("SEX")),
    data_prepare = mk_adae,
    custom_pop_filter = "TRT01A != 'Xanomeline Low Dose'",
    endpoint_filter = "AEDECOD == \"ERYTHEMA\"",
    stat_by_strata_across_trt = list(RD)
  ) |>
    helper_pipeline_by_strata_across_trt()
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
      AEDECOD == "ERYTHEMA"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  expected <- ((a / sum(a, b)) - (c_ / sum(c_, d))) * 100
  expect_equal(actual[label == "RD" &
    strata_var == "TOTAL_"]$value, expected)
  expect_type(actual$value, "double")
})

test_that("RD works when as_pct == FALSE", {
  # SETUP -------------------------------------------------------------------
  ep <- ep_base(
    stratify_by = list(c("SEX")),
    data_prepare = mk_adae,
    custom_pop_filter = "TRT01A != 'Xanomeline Low Dose'",
    endpoint_filter = "AEDECOD == \"ERYTHEMA\"",
    stat_by_strata_across_trt = list(c(RD, as_pct = FALSE))
  ) |>
    helper_pipeline_by_strata_across_trt()
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
      AEDECOD == "ERYTHEMA"] |>
    NROW()

  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]

  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  expected <- ((a / sum(a, b)) - (c_ / sum(c_, d)))
  expect_equal(actual[label == "RD" &
    strata_var == "TOTAL_"]$value, expected)
  expect_type(actual$value, "double")
})

test_that("p-val tests work (barnard)", {
  # SETUP -------------------------------------------------------------------
  ep <- ep_base(
    stratify_by = list(c("SEX")),
    data_prepare = mk_adae,
    custom_pop_filter = "TRT01A != 'Xanomeline Low Dose'",
    endpoint_filter = "AEDECOD == \"ERYTHEMA\"",
    stat_by_strata_across_trt = list(p_val)
  ) |>
    helper_pipeline_by_strata_across_trt()
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
      AEDECOD == "ERYTHEMA"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()
  expected <-
    Barnard::barnard.test(a, c_, b, d)$p.value[2]
  expect_equal(actual[strata_var == "TOTAL_"]$value, expected)
  expect_type(actual$value, "double")
})


test_that("p-val tests work fishers", {
  # SETUP -------------------------------------------------------------------
  ep <- ep_base(
    stratify_by = list(c("SEX")),
    data_prepare = mk_adae,
    custom_pop_filter = "TRT01A != 'Xanomeline Low Dose'",
    endpoint_filter = "AEDECOD == \"ERYTHEMA\"",
    stat_by_strata_across_trt = list(c(
      p_val,
      threshold_lower = 0,
      threshold_upper = 1
    ))
  ) |>
    helper_pipeline_by_strata_across_trt()
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
      AEDECOD == "ERYTHEMA"] |>
    NROW()
  # Outcome, no treatment
  c_ <-
    x[TRT01A == "Placebo" &
      SAFFL == "Y" &
      AEDECOD == "ERYTHEMA"] |>
    NROW()

  x_no_event <- x[!USUBJID %in% x[(has_event), USUBJID]]
  # NO Outcome, on treatment
  b <-
    x_no_event[TRT01A == "Xanomeline High Dose" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()

  # Outcome, no treatment
  d <-
    x_no_event[TRT01A == "Placebo" &
      SAFFL == "Y" &
      (AEDECOD != "ERYTHEMA" |
        is.na(AEDECOD))] |>
    NROW()


  two_by_two <-
    matrix(
      data = c(a, b, c_, d),
      nrow = 2,
      ncol = 2,
      byrow = T
    )
  expected <- stats::fisher.test(x = two_by_two)$p.val
  expect_equal(actual[strata_var == "TOTAL_"]$value, expected)
  expect_type(actual$value, "double")
})
