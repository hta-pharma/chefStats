test_that("n_subj works",
          {
            # SETUP -------------------------------------------------------------------
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              n_subj(dat = input,
                     cell_index = cell_index_f,
                     subjectid_var = "USUBJID")
            
            
            actual_total <-
              n_subj(dat = input,
                     cell_index = cell_index_total,
                     subjectid_var = "USUBJID")
            # EXPECT ------------------------------------------------------------------
            x <- mk_adae()
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            event_index <-
              input[AEDECOD == "ERYTHEMA"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              n_event(
                dat = input,
                cell_index = cell_index_f,
                event_index = event_index,
                subjectid_var = "USUBJID"
              )
            
            
            actual_total <- n_event(
              dat = input,
              cell_index = cell_index_total,
              event_index = event_index,
              subjectid_var = "USUBJID"
            )
            
            # EXPECT ------------------------------------------------------------------
            x <- mk_adae()
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            event_index <-
              input[AEDECOD == "ERYTHEMA"][["INDEX_"]]
            
            
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              p_subj_event(
                dat = input,
                cell_index = cell_index_f,
                event_index = event_index,
                subjectid_var = "USUBJID"
              )
            
            
            actual_total <- p_subj_event(
              dat = input,
              cell_index = cell_index_total,
              event_index = event_index,
              subjectid_var = "USUBJID"
            )
            # EXPECT ------------------------------------------------------------------
            x <- mk_adae()
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo"][["INDEX_"]]
            event_index <-
              input[AESEV == "SEVERE" &
                      AESOC == "GASTROINTESTINAL DISORDERS"][["INDEX_"]]
            # ACT ---------------------------------------------------------------------
            
            
            
            actual_female <-  list(
              p_subj_event =
                p_subj_event(
                  input,
                  cell_index = cell_index_f,
                  event_index = event_index,
                  "USUBJID"
                )$value,
              n_event = n_event(input, cell_index_f, event_index, "USUBJID")$value,
              n_subj_event = n_subj_event(input, cell_index_f, event_index, "USUBJID")$value
            )
            
            actual_total <-  list(
              p_subj_event =
                p_subj_event(
                  input,
                  cell_index = cell_index_total,
                  event_index = event_index,
                  "USUBJID"
                )$value,
              n_event = n_event(input, cell_index_total, event_index, "USUBJID")$value,
              n_subj_event = n_subj_event(input, cell_index_total, event_index, "USUBJID")$value
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              mean_value(
                dat = input,
                event_index = input[["INDEX_"]],
                cell_index = cell_index_f,
                subjectid_var = "USUBJID",
                var = "AGE"
              )
            
            actual_total <-
              mean_value(
                dat = input,
                event_index = input[["INDEX_"]],
                cell_index = cell_index_total,
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              sd_value(
                dat = input,
                event_index = input[["INDEX_"]],
                cell_index = cell_index_f,
                subjectid_var = "USUBJID",
                var = "AGE"
              )
            
            actual_total <-
              sd_value(
                dat = input,
                event_index = input[["INDEX_"]],
                cell_index = cell_index_total,
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            event_index <- which(input$ASEV == "MILD")
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              p_subj_event_by_trt(
                dat = input,
                event_index = event_index,
                cell_index = cell_index_f,
                subjectid_var = "USUBJID",
                treatment_var = "TRT01A",
                treatment_value = "Placebo"
              )
            
            actual_total <-
              p_subj_event_by_trt(
                dat = input,
                event_index = event_index,
                cell_index = cell_index_total,
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


test_that("obs_time_by_trt works",
          {
            # SETUP -------------------------------------------------------------------
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            input[, INTRDURY := TRTDURD]
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              obs_time_by_trt(dat = input,
                              cell_index = cell_index_f,
                              subjectid_var = "USUBJID")
            
            actual_total <-
              obs_time_by_trt(dat = input,
                              cell_index = cell_index_total,
                              subjectid_var = "USUBJID")
            
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
            input <- mk_adae()
            input[, INDEX_ := .I] |> setkey(INDEX_)
            input[, INTRDURY := TRTDURD]
            cell_index_f <-
              input[SAFFL == "Y" &
                      TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
            cell_index_total <-
              input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
            event_index <- which(input$ASEV == "MILD")
            
            # ACT ---------------------------------------------------------------------
            
            actual_female <-
              n_event_100y(
                dat = input,
                event_index = event_index,
                cell_index = cell_index_f,
                subjectid_var = "USUBJID",
                treatment_var = "TRT01A",
                treatment_value = "Placebo"
              )
            
            actual_total <-
              n_event_100y(
                dat = input,
                event_index = event_index,
                cell_index = cell_index_total,
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
                       TRT01A == "Placebo" & ASEV == "MILD" & SEX == "F"])
            expected_female <- round(n_event_f / obs_time * 100)
            expect_equal(actual_female$value, expected_female)
            
            
            n_event_t <-
              nrow(x[SAFFL == "Y" & TRT01A == "Placebo" & ASEV == "MILD"])
            expected_total <- round(n_event_t / obs_time * 100)
            expect_equal(actual_total$value, expected_total)
            
            expect_type(actual_female$value, "double")
          })
