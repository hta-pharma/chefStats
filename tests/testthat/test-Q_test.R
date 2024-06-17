test_that("Q_test", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, 0.8426615, tolerance = 1e-7)
})

test_that("Q_test: records with missing values removed", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$input[4:6, SEX := NA]
  # ACT ------------------------------------------------
  
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, 0.5363668, tolerance = 1e-7)
})


test_that("Q_test: only 1 event in dataset", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$event_index <- 4
  # ACT ------------------------------------------------
  
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, NA_real_)
})


test_that("Q_test: all have events", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$event_index <- 1:nrow(setup$input)
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, NA_real_)
})


test_that("Q_test: very many events", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$event_index <- 1:(nrow(setup$input) / 1.1)
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, 0.5597077, tolerance = 1e-7)
})


test_that("Q_test: extreme imbalanced events distributed across strata", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$event_index <-
    c(which(setup$input$SEX == "M"), which(setup$input$SEX == "F")[1:5])
  
  
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, 0.7674063, tolerance = 1e-7)
})

test_that("Q_test: extreme imbalanced events distributed across TRT", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$event_index <- which(setup$input$TRT01A == "Placebo")
  
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, NA_real_)
})

test_that("Q_test:imbalanced events distributed across TRT", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  setup$event_index <-
    c(which(setup$input$TRT01A == "Placebo")[-1:-3], which(setup$input$TRT01A != "Placebo")[1])
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input,
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID"
  )
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, 0.5235664, tolerance = 1e-7)
})

test_that("Q_test returns NA when strata_var contains only 1 level", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  SEX <- NULL
  # ACT ------------------------------------------------
  actual_total <- Q_test(
    dat = setup$input[SEX == "M"],
    event_index = setup$event_index,
    cell_index = setup$cell_index_total,
    treatment_var = "TRT01A",
    strata_var = "SEX",
    treatment_refval = "Xanomeline High Dose",
    subjectid_var = "USUBJID",
  )
  
  # EXPECT ------------------------------------------------------------------
  expect_equal(actual_total$value, NA_real_)
})


test_that("Q_test errors when only 1 treatment level", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_by_strata_across_trt()
  TRT01A <- NULL
  # ACT ------------------------------------------------
  expect_error(
    Q_test(
      dat = setup$input[TRT01A == "Placebo"],
      event_index = setup$event_index,
      cell_index = setup$cell_index_total,
      treatment_var = "TRT01A",
      strata_var = "SEX",
      treatment_refval = "Xanomeline High Dose",
      subjectid_var = "USUBJID",
    ),
    "make_two_by_two_ only supports"
  )
})
