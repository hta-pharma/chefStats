#' Breslow-Day test
#'
#' @param x a 2x2xK contingency table
#' @param odds_ratio Odds Ration (default = NA)
#' @param correct if TRUE Tarones correction is returned. Default = FALSE.
#'
#' @return A vector with three values statistic - Breslow and Day test
#'   statistic pval - p value evtl. based on the Tarone test statistic using a
#'    \eqn{\chi^2(K-1)} distribution
#' @noRd
breslowdaytest <- function(x, odds_ratio = NA, correct = FALSE) {

  # Function to perform the Breslow and Day (1980) test including the
  # corrected test by Tarone Uses the equations in Lachin (2000),
  # Biostatistical Methods, Wiley, p. 124-125.
  #
  # Programmed by Michael Hoehle <http://www.math.su.se/~hoehle>
  # Code taken originally from a Biostatistical Methods lecture
  # held at the Technical University of Munich in 2008.
  #
  # Params:
  #  x - a 2x2xK contingency table
  #  correct - if TRUE Tarones correction is returned
  #
  # Returns:
  #  a vector with three values
  #   statistic - Breslow and Day test statistic
  #   pval - p value evtl. based on the Tarone test statistic
  #               using a \chi^2(K-1) distribution
  #

  if (is.na(odds_ratio)) {
    #Find the common odds_ratio based on Mantel-Haenszel
    oddsratio_hat_mh <- stats::mantelhaen.test(x)$estimate
  } else {
    oddsratio_hat_mh <- odds_ratio
  }

  #Number of strata
  k <- dim(x)[3]
  #Value of the Statistic
  x2_hbd <- 0
  #Value of aj, tildeaj and variance_aj
  a <- tildea <- variance_a <- numeric(k)

  for (j in 1:k) {
    #Find marginals of table j
    mj <- apply(x[, , j], MARGIN = 1, sum)
    nj <- apply(x[, , j], MARGIN = 2, sum)

    #Solve for tilde(a)_j
    coef <- c(-mj[1] * nj[1] * oddsratio_hat_mh, nj[2] - mj[1]
              + (oddsratio_hat_mh * (nj[1] + mj[1])),
              1 - oddsratio_hat_mh)
    sols <- Re(polyroot(coef))
    #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1], mj[1]))]
    #Observed value
    aj <- x[1, 1, j]

    #Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj

    #Compute \hat{\variance}(a_j | \widehat{\odds_ratio}_MH)
    variance_aj <- (1 / tildeaj + 1 / tildebj + 1 / tildecj + 1 / tildedj)^(-1)

    #Compute contribution
    x2_hbd <- x2_hbd + as.numeric((aj - tildeaj)^2 / variance_aj)

    #Assign found value for later computations
    a[j] <- aj
    tildea[j] <- tildeaj
    variance_a[j] <- variance_aj
  }

  # Compute Tarone corrected test
  # Add on 2015: The original equation from the 2008 lecture is incorrect
  # as pointed out by Jean-Francois Bouzereau.
  x2_hbdt <- as.numeric(x2_hbd -  (sum(a) - sum(tildea))^2 / sum(variance_a))

  dname <- deparse(substitute(x))

  statistic <- if (correct) x2_hbdt else x2_hbd
  parameter <- k - 1
  # Compute p-value based on the Tarone corrected test
  pval <- 1 - stats::pchisq(statistic, parameter)
  method <-
    if (correct)
      "Breslow-Day Test on Homogeneity of Odds Ratios (with Tarone correction)"
    else
      "Breslow-Day Test on Homogeneity of Odds Ratios"
  names(statistic) <- "X-squared"
  names(parameter) <- "df"
  structure(list(statistic = statistic, parameter = parameter,
                 p.value = pval, method = method, data.name = dname),
            class = "htest")

}
