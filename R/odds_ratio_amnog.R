#' Odds Ratio calculation with AMNOG-specific adjustments
#'
#' @description Calculates the Odds Ratio and its 95% CI based off a 2x2 contingency table
#' providing the number of subjects experiencing an outcome vs not experiencing an outcome
#' by two treatment groups.
#'
#' @param two_by_two Two-by-two contingency table which takes the visual form:
#'
#'  |          |Outcome|observed   |
#'  |----------|:-----:|:---------:|
#'  |          |Yes    | No        |
#'  |Treatment |A      |         B |
#'  |Comparator|C      |         D |
#'
#'Where A, B, C, and D are the distinct number of subjects satisfying the criteria of each 2x2 cell.
#'
#' @return A vector with three values, the point estimate, the upper 95%CI, and
#'   the lower 95%CI
#' @noRd

odds_ratio_amnog <- function(two_by_two) {
  checkmate::assertMatrix(two_by_two)
  #AMNOG rule - if any cells are zero, add 0.5 to each cell
  if (prod(two_by_two) == 0) {
    two_by_two <- two_by_two + 0.5
  }

  a <- two_by_two[1, 1] # Disease exposed
  b <- two_by_two[1, 2] # No disease exposed
  c <- two_by_two[2, 1] # Disease not-exposed
  d <- two_by_two[2, 2] # No disease not-exposed
  OR <- (a / b) / (c / d)

  SE <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)

  ORUL <- exp(log(OR) + stats::qnorm(0.975) * SE)

  ORLL <- exp(log(OR) - stats::qnorm(0.975) * SE)

  orset <- as.data.frame(cbind(OR, SE, ORUL, ORLL))

  return(orset)
}
