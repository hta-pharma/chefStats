#' Breslow-Day test
#'
#' @param x a 2x2xK contingency table
#' @param odds_ratio Odds Ration (default = NA)
#' @param correct if TRUE Tarones correction is returned. Default = FALSE.
#'
#' @return A vector with three values statistic - Breslow and Day test
#'   statistic pval - p value evtl. based on the Tarone test statistic using a
#'    \eqn{\chi^2(K-1)} distribution
library(DescTools)
library(testthat)
breslowdaytest_ <- function(x, odds_ratio = NA, correct = FALSE) {

    # Call the BreslowDayTest from the DescTools package
    DescTools::BreslowDayTest(x, odds_ratio, correct)
}
