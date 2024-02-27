#' @title {{fn_name}}
#' @description
#' @param dat
#' @param event_index
#' @param strata_var
#' @param treatment_var
#' @param treatment_refval
#' @param subject_id
#' @param ... Not used by user. Needed to "collect" un-used arguments passed
#'   from chef
#'
#' @return
#' @export
#'
#' @examples
{{fn_name}} <- function(dat,
                        event_index,
                        strata_var,
                        treatment_var,
                        treatment_refval,
                        subject_id,
                        ...) {

  # Function body here:

  # The final object retuned needs to be a data.table with the following format:
  return(
    data.table::data.table(
      label = NA_character_,
      value = NA_real_,
      description = NA_character_
    )
  )

}
