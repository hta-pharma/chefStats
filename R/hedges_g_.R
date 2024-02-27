#' Hedges G
#'
#' @param mean_trt Mean of the treatment group
#' @param mean_comp Mean of the comparator group
#' @param n_trt Number in the treatment group
#' @param n_comp Numeber in the comparator group
#' @param std_trt Standard deviation of the treatment group
#' @param std_comp Standard deviation of the comparator group
#'
#' @return a value
#' @noRd

hedges_g_ <-
  function(mean_trt,
           mean_comp,
           n_trt,
           n_comp,
           std_trt,
           std_comp) {
    x1  <- mean_trt
    x2  <- mean_comp
    n1  <- n_trt
    n2  <- n_comp
    sd1 <- std_trt
    sd2 <- std_comp

    g <-
      ((x1 - x2) / sqrt(((n1 - 1) * (sd1 ^ 2) + (n2 - 1) * (sd2 ^ 2)) / (n1 + n2 -
                                                                           2))) * (1 - 3 / (4 * (n1 + n2) - 9))


    gll <-
      g - stats::qnorm(0.975) * sqrt((n1 + n2) / (n1 * n2) + (g ^ 2) / (2 * (n1 + n2)))
    gul <-
      g + stats::qnorm(0.975) * sqrt((n1 + n2) / (n1 * n2) + (g ^ 2) / (2 * (n1 + n2)))

    gset <- list(g = g, gll = gll, gul = gul)
    return(gset)
  }
