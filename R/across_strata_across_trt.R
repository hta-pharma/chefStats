#' Wrapper for P-value of interaction tests
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param treatment_var character. The name of the treatment variable in the data.
#' @param treatment_refval character. The reference value of the treatment variable in the data.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param strata_var character. Variable in the analysis data to stratify by specific for this call.
#' @param odds_ratio numeric. Odds Ration (default = NA).
#' @param correct logical. If TRUE Tarones correction is returned (default = FALSE).
#' @param ... Optional parameters.
#'
#' @return A data.table containing the statistics for p-value interaction tests.
#' @export
p_val_interaction <- function(dat,
                              event_index,
                              treatment_var,
                              treatment_refval,
                              subjectid_var,
                              strata_var,
                              odds_ratio = NA,
                              correct = FALSE,
                              ...) {
  two_by_two_by_k <-
    make_two_by_two_by_k_(
      dat = dat,
      event_index = event_index,
      strata_var = strata_var,
      treatment_var = treatment_var,
      treatment_refval = treatment_refval,
      subjectid_var = subjectid_var
    )

  valid <- validate_breslow_day(two_by_two_by_k)
  if (valid) {
    stat <- breslowdaytest_(
      two_by_two_by_k,
      odds_ratio = odds_ratio,
      correct = correct
    )
    return(
      data.table(
        label = "P-value interaction",
        description = stat$method,
        qualifiers = NA_character_,
        value = as.double(stat$p.value)
      ))
    
  }

  data.table(
    label = NA_character_,
    description = "P-value interaction not conducted",
    qualifiers = NA_character_,
    value = NA_real_
  )
}

#' Wrapper to prepare data for Hedges G
#'
#' @param dat data.table. The analysis data set.
#' @param reference_val character. The reference value of the treatment variable in the data.
#' @param safe_mode Boolean determining if the function should fail when given
#'   input that cannot be calculated (`safe_mode = TRUE`), or if it should
#'   silently return a `NA` value (default
#' @param ... optional arguments to
#' @return A list containing Hedges G statistics.
#' @export
#'
hedges_g <-
  function(dat, reference_val, safe_mode = FALSE, ...) {
    n_trt <-
      dat[treatment_val != reference_val &
            stat_var == "N_sub", stat_val]
    n_comp <-
      dat[treatment_val == reference_val &
            stat_var == "N_sub", stat_val]

    if (n_trt != 0 & n_comp != 0) {
      mean_trt <-
        dat[treatment_val != reference_val &
              stat_var == "mean_value", stat_val]
      mean_comp <-
        dat[treatment_val == reference_val &
              stat_var == "mean_value", stat_val]
      std_trt <-
        dat[treatment_val != reference_val &
              stat_var == "sd_value", stat_val]
      std_comp <-
        dat[treatment_val == reference_val &
              stat_var == "sd_value", stat_val]

      out <-
        hedges_g_(mean_trt, mean_comp, n_trt, n_comp, std_trt, std_comp)
      return(list(
        pe = out$g,
        upper = out$gul,
        lower = out$gll
      ))
    } else{
      return(list(
        pe = NA,
        upper = NA,
        lower = NA
      ))
    }
  }
