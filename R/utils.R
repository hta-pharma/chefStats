#' A modified min-function that handles cases where the input only contains NAs
#'
#' @param x A vector of numbers possibly containing NAs.
#'
#' @return The minimum value disregarding NAs.
#' @noRd
custom_min <-
  function(x) {
    if (any(!is.na(x)))
      return(min(x, na.rm = T))
    return(as.double(NA))
  }

#' A modified max-function that handles cases where the input only contains NAs.
#'
#' @param x A vector of numbers possibly containing NAs.
#'
#' @return The maximum value disregarding NAs.
#' @noRd
custom_max <-
  function(x) {
    if (any(!is.na(x)))
      return(max(x, na.rm = T))
    return(as.double(NA))
  }


error_name_mismatch <- function(x, fn_name){
  cli::cli_abort(
    c("No matching name in output of {.code {fn_name}} found for {.code x}.",
      x=paste0("Perhaps the names of the object created by ",
               "{.code {fn_name}} have changed")),
    call = NULL
  )
}

