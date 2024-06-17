#' Q test
#'
#' @param dat 
#' @param event_index 
#' @param treatment_var 
#' @param treatment_refval 
#' @param subjectid_var 
#' @param strata_var 
#' @param ... 
#'
#' @return
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
    dat <- dat[get(strata_var)!="" & !is.na(get(strata_var))]
    # dat <-
    #   dat %>% filter(eval(parse(text = strata_var)) != "" &
    #                    !is.na(eval(parse(text = strata_var))))
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
      #Get the dimensions of the 2x2xk contingency
      sg_lvls <- dimnames(two_by_two_by_k)[[3]]
      
      relrisks <- lapply(sg_lvls, function(x) {
        out <- relative_risk_(two_by_two_by_k[, , x])
        return(c(out$RR , out$SE))
      })
      
      names(relrisks) <- sg_lvls
      
      
      #Derive the weights as inverse variance
      
      weights <- lapply(sg_lvls, function(x) {
        V_i <- relrisks[[x]][2]
        w_i <- 1 / (V_i ** 2)
        return(w_i)
      })
      names(weights) <- sg_lvls
      
      #calculate theta_tot
      
      theta_tot_numer <- sum(unlist(lapply(sg_lvls, function(x) {
        ln_RR_i <- log(relrisks[[x]][1])
        w_i <- weights[[x]]
        theta_tot_numer_i <- w_i * ln_RR_i
        return(theta_tot_numer_i)
      })))
      
      theta_tot_denom <- sum(unlist(weights))
      
      theta_tot <- theta_tot_numer / theta_tot_denom
      
      
      #calculate test statistic across subgroups
      
      Q_int <- sum(unlist(lapply(sg_lvls, function(x) {
        ln_RR_i <- log(relrisks[[x]][1])
        w_i <- weights[[x]]
        
        Q_i <- w_i * (ln_RR_i - theta_tot) ** 2
        return(Q_i)
      })))
      
      p_value <- 1 - pchisq(Q_int, length(sg_lvls) - 1)
      
      
      return(
        data.table(
          label = "P-value interaction",
          description = 'Cochrans Q-test',
          qualifiers = NA_character_,
          value = as.double(p_value)
        )
      )
    }
    data.table(
      label = NA_character_,
      description = "P-value interaction not conducted",
      qualifiers = NA_character_,
      value = NA_real_
    )
  }
