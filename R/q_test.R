#' Q test
#' @description This function performs a Cochran's Q test for interaction,
#'   assessing heterogeneity of treatment effects across different strata. It
#'   first creates a 2x2xk contingency table based on the input data and then
#'   calculates the test statistic and p-value for the interaction.
#'
#'   The test is only valid under the following conditions:
#' - The data must contain records in each strata level (defined by `strata_var`).
#' - The `strata_var` variable should define exactly two stratification levels.
#' - Each stratification level should have at least one event.
#' - The product of the sums of events and non-events across treatment groups 
#' within each subgroup should not be zero.
#'
#' @param dat A data frame containing the study data.
#' @param event_index The name of the column in 'dat' that indicates whether the
#'   event of interest has occurred.
#' @param treatment_var The name of the column in 'dat' that specifies the
#'   treatment group.
#' @param treatment_refval The reference value in 'treatment_var' indicating the
#'   control group.
#' @param subjectid_var The name of the column in 'dat' that identifies unique
#'   subjects.
#' @param strata_var The name of the column in 'dat' used for stratifying the
#'   analysis.
#' @param ...
#'
#' @return A data.table containing the label, description, qualifiers, and
#'   p-value of the Cochran's Q test for interaction.
#' @export
#' @export
#'
#' @examples
Q_test <-
  function (dat,
            event_index,
            treatment_var,
            treatment_refval,
            subjectid_var,
            strata_var,
            ...)
  {
    #Remove the missing subgroup levels
    dat <- dat[get(strata_var) != "" & !is.na(get(strata_var))]
    
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
    
    if (!valid) {
      return(
        data.table(
          label = NA_character_,
          description = "P-value interaction not conducted",
          qualifiers = NA_character_,
          value = NA_real_
        )
      )
    }
    
    
    #Get the dimensions of the 2x2xk contingency
    subgroup_levels <- dimnames(two_by_two_by_k)[[3]]
    relrisks <- vapply(subgroup_levels, function(x) {
      out <- relative_risk_(two_by_two_by_k[, , x])
      log_PE <- log(out$RR)
      return(c("PE"=out$RR , "SE"=out$SE, "log_PE"=log_PE))
    },FUN.VALUE = double(length = 3L))
    
    
    # Derive the weights as inverse variance
    weights <- 1 / (relrisks["SE", ]^ 2)
    
    #calculate theta_tot
    theta_tot_numerator <- sum(weights * relrisks["log_PE",])
    
    theta_tot_denom <- weights  |> sum()
    theta_tot <- theta_tot_numerator / theta_tot_denom
    
    Q_int <- (weights * (relrisks["log_PE",] - theta_tot)^2) |> sum()
    #calculate test statistic across subgroups
    
    p_value <- 1 - pchisq(q = Q_int, df = length(subgroup_levels) - 1)
    data.table(
      label = "P-value interaction",
      description = 'Cochrans Q-test',
      qualifiers = NA_character_,
      value = as.double(p_value)
    )
  }
