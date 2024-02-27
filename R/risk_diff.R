#' Risk Difference calculation with AMNOG-specific adjustments
#' @description Calculates the Risk Difference and its 95% CI based off a 2x2 contingency table
#' providing the number of subjects experiencing an outcome vs not experiencing an outcome
#' by two treatment groups.
#' @param as_pct Return results as a percentage (default). Otherwise returns as
#'   fracton.
#' @param two_by_two Two-by-two contingency table which takes the visual form:
#'
#'  |          |Outcome|observed   |
#'  |----------|:-----:|:---------:|
#'  |          |Yes    | No        |
#'  |Treatment |A      |         B |
#'  |Comparator|C      |         D |
#'
#'Where A, B, C, and D are the distinct number of subjects satisfying the criteria of each 2x2 cell.
#' @return A vector with three values, the point estimate, the upper 95%CI, and
#'   the lower 95%CI
#'
#' @noRd

risk_diff <- function(two_by_two, as_pct = TRUE) {
  a <- two_by_two[1, 1] # Disease exposed
  b <- two_by_two[1, 2] # No disease exposed
  c <- two_by_two[2, 1] # Disease not-exposed
  d <- two_by_two[2, 2] # No disease not-exposed

  RD <-
    (a / sum(a, b)) - (c / sum(c, d))

  SE <-
    sqrt(a * b / (sum(a, b) ^ 3)  +  c * d / (sum(c, d) ^ 3))

  RDLL <- RD - stats::qnorm(0.975) * SE
  RDUL <- RD + stats::qnorm(0.975) * SE

  rdset <- as.data.frame(cbind(RD, SE, RDUL, RDLL))

  if (as_pct) {
    rdset[c('RD', 'RDUL', 'RDLL')] <- rdset[c('RD', 'RDUL', 'RDLL')] * 100
  }

  return(rdset)
}
