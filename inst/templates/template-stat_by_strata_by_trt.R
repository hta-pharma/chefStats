#' @title {{fn_name}}
#' @description
#' @param dat
#' @param event_index
#' @param cell_index
#' @param strata_var
#' @param strata_val
#' @param treatment_var
#' @param treatment_val
#' @param subject_id
#' @param ... Not used by user. Needed to "collect" un-used arguments passed
#'   from chef
#'
#' @return
#' @export
#'
#' @examples
{
  {
    fn_name
  }
} <- function(dat,
              event_index,
              cell_index,
              strata_var,
              strata_val,
              treatment_var,
              treatment_val,
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
