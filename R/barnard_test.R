#' Barnards Unconditional Exact Test with trial data
#'
#' @param two_by_two A two-by-two table where the first column contains counts
#'   of events.
#' @param verbose Set to TRUE if you want the normal output from running
#'   barnard.test to print to the console. Default is FALSE
#' @param safe_mode If invalid input is provided, should the function stop with
#'   error (default), or return an NA (safe_mode = FALSE)
#'
#' @return 2-sided p-value for the Barnards Unconditional Exact test
#' @importFrom Barnard barnard.test

barnard_test_ <-
  function(two_by_two,
           verbose = FALSE,
           safe_mode = TRUE) {
    a <- two_by_two[1, 1] # Disease exposed
    b <- two_by_two[1, 2] # No disease exposed
    c <- two_by_two[2, 1] # Disease not-exposed
    d <- two_by_two[2, 2] # No disease not-exposed
    #If the sum of any row or column is non-zero, calculate Barnards Exact Test
    sumprod_test <- prod(sum(a, b),
                         sum(a, c),
                         sum(b, d),
                         sum(c, d))

    valid <- sumprod_test > 0
    if (!valid) {
      if (safe_mode) {
        stop(
          "Barnards unconditional exact test could not be calculated.",
          " At least one row- or column-total is zero."
        )
      }
      return(NA)
    }

    if (verbose) {
      return(Barnard::barnard.test(a, c, b, d)$p.value[2])
    }

    # Because barnard.test uses cat() to print messages, instead of message(),
    # we need to use this construction to avoid printing a bunch of output to
    # the console.
    invisible(utils::capture.output(
      out <- Barnard::barnard.test(a, c, b, d)
    ))
    return(out$p.value[2])
  }
