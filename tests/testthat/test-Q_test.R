test_that("Q_test",{
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
  
  expect_equal(actual_total$value, 0.8426615,tolerance = 1e-7)
})

test_that("Q_test",{
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
    subjectid_var = "USUBJID",correct = 
    
  )
  
  expect_equal(actual_total$value, 0.8426615,tolerance = 1e-7)
})
