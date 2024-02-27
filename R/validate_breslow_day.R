#' Validate inputs to breslow-day function
#'
#' @param two_by_two_by_k A two-by-two-by-k table, usually produced
#' by `make_two_by_two_by_k()`.
#'
#' @return A Boolean TRUE/FALSE
#' @noRd
validate_breslow_day <- function(two_by_two_by_k) {
  n_subgroup_levels <- dim(two_by_two_by_k)[3]
  if(n_subgroup_levels!=2) {
    return(FALSE)
  }
  empty_subgroups_exist <-
    any(colSums(two_by_two_by_k[, 1, 1:n_subgroup_levels]) == 0)
  if (empty_subgroups_exist) {
    return(FALSE)
  }
  sumprod_test <-
    prod(
      sum(two_by_two_by_k[1, 1, 1:n_subgroup_levels]),
      sum(two_by_two_by_k[1, 2, 1:n_subgroup_levels]),
      sum(two_by_two_by_k[2, 1, 1:n_subgroup_levels]),
      sum(two_by_two_by_k[2, 2, 1:n_subgroup_levels])
    )
  if (sumprod_test == 0) {
    return(FALSE)
  }
  return(TRUE)
}
