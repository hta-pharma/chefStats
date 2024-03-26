#' Relative Risk calculation
#' @description Ensures the input to the statistical function is in the proper
#'   format, and ensure the output is formatted to the need of the AMNOG
#'   workflow. 
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog}
#'   for more information.
#' @param treatment_var character. The name of the treatment variable in the data.
#' @param treatment_refval character. The reference value of the treatment variable in the data.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#' 
#' @return A data.table containing Relative Risk statistics.
#' @export
RR <- function(dat,
               event_index,
               cell_index,
               treatment_var,
               treatment_refval,
               subjectid_var,
               ...) {
  # Test a 2x2 contingency table
  two_by_two <-
    make_two_by_two_(
      dat = dat,
      event_index = event_index,
      cell_index = cell_index,
      treatment_var = treatment_var,
      treatment_refval = treatment_refval,
      subjectid_var = subjectid_var
    )
  out <- relative_risk_(two_by_two)
  desc <-
    vapply(names(out), function(x) {
      switch(x,
        "RR" = "Relative Risk",
        "RRLL" = "Relative Risk 95%-CI lower limit",
        "RRUL" = "Relative Risk 95%-CI upper limit",
        "SE" = "Relative Risk standard error",
        error_name_mismatch(x, "relative_risk_()")
      )
    }, FUN.VALUE = character(1L))

  data.table(
    label = names(out),
    description = desc,
    qualifiers = NA_character_,
    value = as.double(out)
  )
}

#' Wrapper for Odds Ratio calculation
#' @description Ensures the input to the statistical function is in the proper
#'   format, and ensures the output is formatted to the need of the AMNOG
#'   workflow.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog}
#'   for more information.
#' @param treatment_var character. The name of the treatment variable in the data.
#' @param treatment_refval character. The reference value of the treatment variable in the data.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#'
#' @return A data.table containing Odds Ratio statistics.
#' @export
OR <- function(dat,
               event_index,
               cell_index,
               treatment_var,
               treatment_refval,
               subjectid_var,
               ...) {
  # Test a 2x2 contingency table
  two_by_two <-
    make_two_by_two_(
      dat = dat,
      event_index = event_index,
      cell_index = cell_index,
      treatment_var = treatment_var,
      treatment_refval = treatment_refval,
      subjectid_var = subjectid_var
    )
  out <- odds_ratio_amnog(two_by_two = two_by_two)
  desc <-
    vapply(names(out), function(x) {
      switch(x,
        "OR" = "Odds Ratio",
        "ORLL" = "Odds Ratio 95%-CI lower limit",
        "ORUL" = "Odds Ratio 95%-CI upper limit",
        "SE" = "Odds Ratio standard error",
        error_name_mismatch(x, "odds_ratio_amnog()")
      )
    }, FUN.VALUE = character(1L))

  return(data.table(
    label = names(out),
    description = desc,
    qualifiers = NA_character_,
    value = as.double(out)
  ))
}


#' Wrapper for Risk Difference calculation
#' @description Ensures the input to the statistical function is in the proper
#'   format, and ensures the output is formatted to the need of the AMNOG
#'   workflow.
#'   
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog}
#'   for more information.
#' @param treatment_var character. The name of the treatment variable in the data.
#' @param treatment_refval character. The reference value of the treatment variable in the data.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param as_pct Boolean. 
#' @param ... Optional parameters.
#'
#' @return A data.table containing Risk Difference statistics.
#' @export
RD <- function(dat,
               event_index,
               cell_index,
               treatment_var,
               treatment_refval,
               subjectid_var,
               as_pct = TRUE,
               ...) {
  # Test a 2x2 contingency table
  two_by_two <-
    make_two_by_two_(
      dat = dat,
      event_index = event_index,
      cell_index = cell_index,
      treatment_var = treatment_var,
      treatment_refval = treatment_refval,
      subjectid_var = subjectid_var
    )
  out <- risk_diff(two_by_two = two_by_two, as_pct = as_pct)
  desc <-
    vapply(names(out), function(x) {
      switch(x,
        "RD" = "Risk Difference",
        "RDLL" = "Risk Difference 95%-CI lower limit",
        "RDUL" = "Risk Difference 95%-CI upper limit",
        "SE" = "Risk Difference standard error",
        error_name_mismatch(x, "risk_diff()")
      )
    }, FUN.VALUE = character(1L))

  return(data.table(
    label = names(out),
    description = desc,
    qualifiers = NA_character_,
    value = as.double(out)
  ))
}


#' Wrapper for P-value calculations
#' @description Ensures the input to the statistical function is in the proper
#'   format, and enruse the output is formated to the need of the AMNOG
#'   workflow.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog}
#'   for more information.
#' @param safe_mode Boolean determing if the function should fail when given
#'   input that cannot be calculated (`safe_mode = TRUE`), or if it should silently return a `NA` value (default).
#' @param treatment_var character. The name of the treatment variable in the data.
#' @param treatment_refval character. The reference value of the treatment variable in the data.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param threshold_lower numeric. Lower threshold limit for selecting Fischer vs. Barnard.
#' @param threshold_upper numeric. Upper threshold limit for selecting Fischer vs. Barnard.
#' @param ... Optional parameters.  
#' @return A data.table containing p-value statistics.
#' @export
p_val <-
  function(dat,
           event_index,
           cell_index,
           treatment_var,
           treatment_refval,
           subjectid_var,
           safe_mode = FALSE,
           threshold_lower = 5,
           threshold_upper = 200,
           ...) {
    two_by_two <-
      make_two_by_two_(
        dat = dat,
        event_index = event_index,
        cell_index = cell_index,
        treatment_var = treatment_var,
        treatment_refval = treatment_refval,
        subjectid_var = subjectid_var
      )

    test_method <-
      fisher_vs_barnard(two_by_two, threshold_lower, threshold_upper)
    if (test_method == "barnard") {
      message("----- Calculating Barnards test - this may take a minute \n")
    }
    pval <- switch(test_method,
      "barnard" = barnard_test_(two_by_two = two_by_two, safe_mode = safe_mode),
      "fisher" = stats::fisher.test(two_by_two)$p.val
    )

    return(data.table(
      label = "p-value",
      description = ifelse(
        test_method == "barnard",
        "Barnard's test",
        "Fisher's exact test"
      ),
      qualifiers = NA_character_,
      value = as.double(pval)
    ))
  }
