#' Make a two-by-two table
#' @description Makes a two-by-two contingency table which takes the visual form:
#'
#'  |          |Outcome|observed   |
#'  |----------|:-----:|:---------:|
#'  |          |Yes    | No        |
#'  |Treatment |A      |         B |
#'  |Comparator|C      |         D |
#'
#'  Where A, B, C, and D are the distinct number of subjects satisfying the
#'  criteria of each 2x2 cell.
#'
#'  Only observations that have a Treatment value recorded are returned.

#'@param dat The ADaM table produced by baker
#'@param treatment_var character. The name of the treatment variable
#'@param treatment_refval character. The reference value of the treatment
#'@param keepflg_var character. The name of the variable in the enriched ADaM
#'  data.table that contains the flag indicating whether each record fulfilled
#'  the criteria for being considered an event for the specific endpoint
#'
#' @return A matrix
#' @export
#'
mk_two_by_two <-
  function(dat,
           event_index,
           cell_index,
           treatment_var,
           treatment_refval,
           subjectid_var) {
    dat_ <- copy(dat)
    n_trt_levels <-
      dat[, unique(dat, by = treatment_var)][[treatment_var]] |>
      length()
    if (n_trt_levels != 2) {
      stop(
        "mk_two_by_two only supports copmarison between two treatment levels. ",
        "This dataset has ",
        n_trt_levels,
        " treatment levels"
      )
    }
    # Subjects who experienced an event will have >1 record. But we need only 1
    # record per subject. So we set the order so that for subjects with >1
    # record, the record containing the event will come first. That way, when we
    # take the unique rows, we preserve the information that the subject
    # experienced the event
    dat_[, is_event := INDEX_ %in% event_index]
    dat_[, is_cell := INDEX_ %in% cell_index]
    setorderv(dat_, c("is_cell", "is_event"), order = -1L) # NOTE: `order = -1L` means row with TRUE come first

    # We don't want to know how many times each subject had an event, only if
    # they had one or not.
    dat_unique <-
      unique(dat_, by = subjectid_var) |>
      data.table::setkeyv(c("is_event", "is_cell", treatment_var))

    # CJ() allows us to aggregate while keeping the 0s
    two_by_two_long <-
      dat_unique[data.table::CJ(is_event, is_cell, get(treatment_var), unique = TRUE),
                 .N,
                 by = .EACHI]

    two_by_two_long_all <-
      ensure_complete_two_by_two(two_by_two_long, treatment_var)
    # Reshape and rename
    two_by_two_ <-
      two_by_two_long_all[(is_cell) |
                            is.na(is_cell)][, is_cell := NULL] |>
      data.table::setnames(old = treatment_var, new = "treatment")
    if (nrow(two_by_two_) > 4) {
      stop("Malformed 2x2 table")
    }
    two_by_two_ <-
      data.table::dcast.data.table(two_by_two_, treatment ~ is_event, value.var = "N")

    # Reorder rows to ensure the reference treatment is the first row and column
    # order is standardized
    dt_match <- two_by_two_[treatment == treatment_refval]
    dt_rest <- two_by_two_[treatment != treatment_refval]
    two_by_two <- rbind(dt_match, dt_rest) |>
      data.table::setnames(old = c("FALSE", "TRUE"),
               new = c("outcome_NO", "outcome_YES")) |>
      data.table::setcolorder(neworder = c("treatment", "outcome_YES", "outcome_NO"))

    out <- as.matrix(two_by_two[, !"treatment"])
    rownames(out) <- two_by_two$treatment

    out[]
  }


#' Ensure complete 2 x 2
#' @details Ensure event column is included in 2x2 table when no events occur in
#'   x
#' @param x data.table
#'
#' @return data.table of 2x2 table in long format
#' @noRd
ensure_complete_two_by_two <- function(two_by_two_long, treatment_var) {
  n_rows_two_by_two_long <-
    two_by_two_long[(is_cell), .N, by = c("is_event", treatment_var)] |> NROW()

  if (n_rows_two_by_two_long == 4) {
    return(two_by_two_long)
  }

  x_grid <-
    expand.grid(is_event = c(TRUE, FALSE), unique(two_by_two_long[[treatment_var]])) |>
    data.table::setDT() |>
    data.table::setnames("Var2", treatment_var)
  out <-
    two_by_two_long[x_grid, on = c("is_event", treatment_var), nomatch = NA]
  out[is.na(N), N := 0]
  out[]
}

#' Make 2x2xk contingency tables from summarized adam data. This function is NOT
#' generalized
#'
#'
#' @param dat data.table. An enriched ADaM table
#' @param treatment_refval character. The treatment reference group
#' @param treatment_var character. The name of the column containing the
#'   treatment value
#' @param subjectid_var character. The name of the column in the enriched ADaM
#'   dataset containing the unique subject id (default = `USUBJID`).
#' @param event_index
#' @param cell_index
#' @param strata_var
#'
#' @return A two-by-two-by-k array where k represents the number of subgroups
#'   (strata).
#' @export
make_two_by_two_by_k <-
  function(dat,
           event_index,
           strata_var,
           treatment_var,
           treatment_refval,
           subjectid_var) {
    # Cell index is not relevant for `across_strata_across_trt`, but to simplify
    # the code we make a dummy cell_index to pass to mk_two_by_two
    cell_index <- dat$INDEX_

    x <- lapply(
      split(dat, by = strata_var),
      mk_two_by_two,
      event_index,
      cell_index,
      treatment_var,
      treatment_refval,
      subjectid_var
    )

    # Simplify2array will behave unexpectedly if input matrices do all not have
    # correct dimensions
    lapply(x, function(i)
      all(dim(i) == 2))

    simplify2array(x)
  }
