# test_that("Demographics (continuous) work when no strata level provided", {
#   # SETUP -------------------------------------------------------------------
#   ep  <- chef::mk_endpoint_str(
#     data_prepare = mk_adlb,
#     treatment_var = "TRT01A",
#     treatment_refval = "Xanomeline High Dose",
#     pop_var = "SAFFL",
#     pop_value = "Y",
#     group_by = list(list(AVISIT = c())),
#     stratify_by = list(c("AGEGR1")),
#     stat_by_strata_by_trt = list(
#       "mean: VALUE_CHANGE" = c(chefStats::mean_value, var = "VALUE_CHANGE"),
#       "sd: VALUE_CHANGE" = c(chefStats::sd_value, var = "VALUE_CHANGE")
#     ),
#     endpoint_label = "Baseline and change from baseline on SODIUM - <AVISIT>"
#   ) |>
#     helper_pipeline()
#
#   # ACT ---------------------------------------------------------------------
#   actual <-
#     chef::apply_stats(ep$ep,
#                       ep$analysis_data_container,
#                       type = "stat_by_strata_by_trt") |>
#     tidyr::unnest(cols = stat_result) |> setDT()
#   browser()
#   a <-
#     actual[label == "mean", .(stat_filter, fn_name, label, value)] |>  setorder(stat_filter)
#   # EXPECT ------------------------------------------------------------------
#
#
#   x <- mk_advs()
#
#   x <- x  |> unique(by = "USUBJID")
#
#   mean_age <- x[, mean(AGE), by = TRT01A]
#   mean_bw <- x[, mean(BW_BASELINE), by = TRT01A]
#
#   expect_equal(a[grepl("AGE", fn_name), value], mean_age[["V1"]], ignore_attr =
#                  TRUE)
#   expect_equal(a[grepl("BW", fn_name), value], mean_bw[["V1"]], ignore_attr =
#                  TRUE)
# })
#
