test_that("n_subj works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              n_subj(
                dat = setup$input,
                cell_index = setup$cell_index_f,
                subjectid_var = "USUBJID"
              )
            
            
            actual_total <-
              n_subj(
                dat = setup$input,
                cell_index = setup$cell_index_total,
                subjectid_var = "USUBJID"
              )
            # EXPECT ------------------------------------------------------------------
            x <- mk_adae(study_metadata = NULL)
            expected_female <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" &  SEX == "F"] |>
              uniqueN(by = "USUBJID")
            expected_total <-
              x[TRT01A == "Placebo" & SAFFL == "Y"] |>
              uniqueN(by = "USUBJID")
            
            expect_equal(actual_female$value, expected_female)
            
            expect_equal(actual_total$value, expected_total)
            expect_type(actual_total$value, "double")
          })


test_that("n_event works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              n_event(
                dat = setup$input,
                cell_index = setup$cell_index_f,
                event_index = setup$event_index,
                subjectid_var = "USUBJID"
              )
            
            
            actual_total <- n_event(
              dat = setup$input,
              cell_index = setup$cell_index_total,
              event_index = setup$event_index,
              subjectid_var = "USUBJID"
            )
            
            # EXPECT ------------------------------------------------------------------
            x <- mk_adae(study_metadata = NULL)
            expected_total <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" & AEDECOD == "ERYTHEMA"] |>
              nrow()
            expected_female <-
              x[TRT01A == "Xanomeline High Dose" &
                  SAFFL == "Y" &
                  SEX == "F" & AEDECOD == "ERYTHEMA"] |>
              nrow()
            expect_equal(actual_total$value, expected_total)
            expect_equal(actual_female$value,
                         expected_female)
            expect_type(actual_female$value, "double")
          })

test_that("p_subj_event works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              p_subj_event(
                dat = setup$input,
                cell_index = setup$cell_index_f,
                event_index = setup$event_index,
                subjectid_var = "USUBJID"
              )
            
            
            actual_total <- p_subj_event(
              dat = setup$input,
              cell_index = setup$cell_index_total,
              event_index = setup$event_index,
              subjectid_var = "USUBJID"
            )
            # EXPECT ------------------------------------------------------------------
            x <- mk_adae(study_metadata = NULL)
            expected_total_numerator <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" &  AEDECOD == "ERYTHEMA"] |>
              uniqueN(by = "USUBJID")
            expected_total_denomenator <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y"] |>
              uniqueN(by = "USUBJID")
            expected_total <-
              expected_total_numerator / expected_total_denomenator * 100
            
            expected_female_numerator <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" &
                  SEX == "F" & AEDECOD == "ERYTHEMA"] |>
              uniqueN(by = "USUBJID")
            expected_female_denomenator <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" &
                  SEX == "F"] |>
              uniqueN(by = "USUBJID")
            expected_female <-
              expected_female_numerator / expected_female_denomenator * 100
            
            expect_equal(actual_total$value, expected_total)
            expect_equal(actual_female$value,
                         expected_female)
            expect_type(actual_total$value, "double")
          })


test_that("n_event, n_subj_event, p_subj_event return 0 when no events",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            setup$event_index <-
              setup$input[AESEV == "SEVERE" &
                            AESOC == "GASTROINTESTINAL DISORDERS"][["INDEX_"]]
            # ACT ---------------------------------------------------------------------
            
            
            
            actual_female <-  list(
              p_subj_event =
                p_subj_event(
                  setup$input,
                  cell_index = setup$cell_index_f,
                  event_index = setup$event_index,
                  "USUBJID"
                )$value,
              n_event = n_event(
                setup$input,
                setup$cell_index_f,
                setup$event_index,
                "USUBJID"
              )$value,
              n_subj_event = n_subj_event(
                setup$input,
                setup$cell_index_f,
                setup$event_index,
                "USUBJID"
              )$value
            )
            
            actual_total <-  list(
              p_subj_event =
                p_subj_event(
                  setup$input,
                  cell_index = setup$cell_index_total,
                  event_index = setup$event_index,
                  "USUBJID"
                )$value,
              n_event = n_event(
                setup$input,
                setup$cell_index_total,
                setup$event_index,
                "USUBJID"
              )$value,
              n_subj_event = n_subj_event(
                setup$input,
                setup$cell_index_total,
                setup$event_index,
                "USUBJID"
              )$value
            )
            # EXPECT ------------------------------------------------------------------
            
            expect_equal(unlist(actual_female), c(0, 0, 0), ignore_attr =
                           TRUE)
            expect_equal(unlist(actual_total), c(0, 0, 0), ignore_attr =
                           TRUE)
          })


test_that("mean_value works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              mean_value(
                dat = setup$input,
                event_index = setup$input[["INDEX_"]],
                cell_index = setup$cell_index_f,
                subjectid_var = "USUBJID",
                var = "AGE"
              )
            
            actual_total <-
              mean_value(
                dat = setup$input,
                event_index = setup$input[["INDEX_"]],
                cell_index = setup$cell_index_total,
                subjectid_var = "USUBJID",
                var = "AGE"
              )
            
            # EXPECT ------------------------------------------------------------------
            
            x <- mk_adae()
            
            x_f <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" &
                  SEX == "F"] |> unique(by = "USUBJID")
            expected_female <-  mean(x_f[["AGE"]])
            expect_equal(actual_female$value, expected_female)
            
            x_t <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y"] |> unique(by = "USUBJID")
            expected_total <-  mean(x_t[["AGE"]])
            expect_equal(actual_total$value, expected_total)
            
            expect_type(actual_total$value, "double")
          })

test_that("sd_value works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              sd_value(
                dat = setup$input,
                event_index = setup$input[["INDEX_"]],
                cell_index = setup$cell_index_f,
                subjectid_var = "USUBJID",
                var = "AGE"
              )
            
            actual_total <-
              sd_value(
                dat = setup$input,
                event_index = setup$input[["INDEX_"]],
                cell_index = setup$cell_index_total,
                subjectid_var = "USUBJID",
                var = "AGE"
              )
            
            # EXPECT ------------------------------------------------------------------
            
            x <- mk_adae()
            
            x_f <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y" &
                  SEX == "F"] |> unique(by = "USUBJID")
            expected_female <-  sd(x_f[["AGE"]])
            expect_equal(actual_female$value, expected_female)
            
            x_t <-
              x[TRT01A == "Placebo" &
                  SAFFL == "Y"] |> unique(by = "USUBJID")
            expected_total <-  sd(x_t[["AGE"]])
            expect_equal(actual_total$value, expected_total)
            
            expect_type(actual_total$value, "double")
          })


test_that("p_subj_event_by_trt works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            setup$event_index <- which(setup$input$ASEV == "MILD")
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              p_subj_event_by_trt(
                dat = setup$input,
                event_index = setup$event_index,
                cell_index = setup$cell_index_f,
                subjectid_var = "USUBJID",
                treatment_var = "TRT01A",
                treatment_value = "Placebo"
              )
            
            actual_total <-
              p_subj_event_by_trt(
                dat = setup$input,
                event_index = setup$event_index,
                cell_index = setup$cell_index_total,
                subjectid_var = "USUBJID",
                treatment_var = "TRT01A",
                treatment_value = "Placebo"
              )
            
            # EXPECT ------------------------------------------------------------------
            
            x <- mk_adae()
            
            n_sub <- x[TRT01A == "Placebo" & SAFFL == "Y"] |>
              unique(by = c("USUBJID")) |>
              NROW()
            
            n_subev_f <-
              x[SAFFL == "Y" &
                  TRT01A == "Placebo" &
                  ASEV == "MILD" & SEX == "F"]  |>
              unique(by = c("USUBJID")) |>
              NROW()
            expected_female <- n_subev_f / n_sub * 100
            
            n_subev_t <-
              x[SAFFL == "Y" &
                  TRT01A == "Placebo" & ASEV == "MILD"]  |>
              unique(by = c("USUBJID")) |>
              NROW()
            expected_total <- n_subev_t / n_sub * 100
            
            expect_equal(actual_female$value, expected_female)
            expect_equal(actual_total$value, expected_total)
            
            expect_type(actual_female$value, "double")
          })

test_that("count_set works", {
  # SETUP -------------------------------------------------------------------
  setup <- setup_basic_counts()
  # ACT ---------------------------------------------------------------------
  actual_female <-
    count_set(
      dat = setup$input,
      cell_index = setup$cell_index_f,
      event_index = setup$event_index,
      subjectid_var = "USUBJID"
    )
  
  
  actual_total <- count_set(
    dat = setup$input,
    cell_index = setup$cell_index_total,
    event_index = setup$event_index,
    subjectid_var = "USUBJID"
  )
  # EXPECT ------------------------------------------------------------------
  x <- mk_adae(study_metadata = NULL)
  
  # Totals
  exepected_total_E <- x[TRT01A == "Placebo" &
                     SAFFL == "Y" &
                     AEDECOD == "ERYTHEMA"] |>
    NROW()
  expected_total_numerator <-
    x[TRT01A == "Placebo" &
        SAFFL == "Y" &  AEDECOD == "ERYTHEMA"] |>
    uniqueN(by = "USUBJID")
  expected_total_denomenator <-
    x[TRT01A == "Placebo" &
        SAFFL == "Y"] |>
    uniqueN(by = "USUBJID")
  expected_total <-
    expected_total_numerator / expected_total_denomenator * 100
  
  # Female
  expected_female_E <- x[TRT01A == "Placebo" &
                            SAFFL == "Y" &
                            SEX == "F" &
                            AEDECOD == "ERYTHEMA"] |>
    NROW()
  expected_female_numerator <-
    x[TRT01A == "Placebo" &
        SAFFL == "Y" &
        SEX == "F" & AEDECOD == "ERYTHEMA"] |>
    uniqueN(by = "USUBJID")
  expected_female_denomenator <-
    x[TRT01A == "Placebo" &
        SAFFL == "Y" &
        SEX == "F"] |>
    uniqueN(by = "USUBJID")
  expected_female <-
    expected_female_numerator / expected_female_denomenator * 100
  
  
  expect_equal(actual_total[label == "N"]$value, expected_total_denomenator)
  expect_equal(actual_total[label == "n"]$value, expected_total_numerator)
  expect_equal(actual_total[label == "E"]$value, exepected_total_E)
  expect_equal(actual_total[label == "(%)"]$value, expected_total)
  
  expect_equal(actual_female[label == "N"]$value, expected_female_denomenator)
  expect_equal(actual_female[label == "n"]$value, expected_female_numerator)
  expect_equal(actual_female[label == "E"]$value, expected_female_E)
  expect_equal(actual_female[label == "(%)"]$value,
               expected_female)
  
})


test_that("obs_time_by_trt works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            setup$input[, INTRDURY := TRTDURD]
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              obs_time_by_trt(
                dat = setup$input,
                cell_index = setup$cell_index_f,
                subjectid_var = "USUBJID"
              )
            
            actual_total <-
              obs_time_by_trt(
                dat = setup$input,
                cell_index = setup$cell_index_total,
                subjectid_var = "USUBJID"
              )
            
            # EXPECT ------------------------------------------------------------------
            
            x <- mk_adae()
            x[, INTRDURY := TRTDURD]
            
            x_f <-
              x[SAFFL == "Y" & TRT01A == "Placebo" & SEX == "F"] |>
              unique(by = c("USUBJID"))
            expected_female <- sum(x_f[["INTRDURY"]], na.rm = TRUE)
            expect_equal(actual_female$value, expected_female)
            
            x_t <- x[SAFFL == "Y" & TRT01A == "Placebo"] |>
              unique(by = c("USUBJID"))
            expected_total <- sum(x_t[["INTRDURY"]], na.rm = TRUE)
            expect_equal(actual_total$value, expected_total)
            
            expect_type(actual_female$value, "double")
          })


test_that("n_event_100y works",
          {
            # SETUP -------------------------------------------------------------------
            setup <- setup_basic_counts()
            setup$input[, INTRDURY := TRTDURD]
            setup$event_index <- which(setup$input$ASEV == "MILD")
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              n_event_100y(
                dat = setup$input,
                event_index = setup$event_index,
                cell_index = setup$cell_index_f,
                subjectid_var = "USUBJID",
                treatment_var = "TRT01A",
                treatment_value = "Placebo"
              )
            
            actual_total <-
              n_event_100y(
                dat = setup$input,
                event_index = setup$event_index,
                cell_index = setup$cell_index_total,
                subjectid_var = "USUBJID",
                treatment_var = "TRT01A",
                treatment_value = "Placebo"
              )
            
            # EXPECT ------------------------------------------------------------------
            
            x <- mk_adae()
            x[, INTRDURY := TRTDURD]
            
            x_sub <- x[SAFFL == "Y" & TRT01A == "Placebo"] |>
              unique(by = c("USUBJID"))
            obs_time <- sum(x_sub[["INTRDURY"]], na.rm = TRUE)
            
            
            n_event_f <-
              nrow(x[SAFFL == "Y" &
                       TRT01A == "Placebo" &
                       ASEV == "MILD" & SEX == "F"])
            expected_female <- round(n_event_f / obs_time * 100)
            expect_equal(actual_female$value, expected_female)
            
            
            n_event_t <-
              nrow(x[SAFFL == "Y" &
                       TRT01A == "Placebo" & ASEV == "MILD"])
            expected_total <- round(n_event_t / obs_time * 100)
            expect_equal(actual_total$value, expected_total)
            
            expect_type(actual_female$value, "double")
          })
