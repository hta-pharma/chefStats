#' @title Select Fisher or Barnard test
#' @description Helper function to  select either Fisher or Barnard's test based
#'   on the data. Rules for statistical test should come from regulatory
#'   document.
#'
#' @param event_counts Number of event.
#' @noRd
fisher_vs_barnard <-
  function(event_counts,
           threshold_lower = 5,
           threshold_upper = 200) {
    checkmate::assert_numeric(event_counts)
    data.table::fcase(
      min(event_counts) <= threshold_lower |
        sum(event_counts) <= threshold_upper,
      "barnard",
      default = "fisher"
    )

  }
