#' Building-block: Number of subjects
#'
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog} for more
#'   information.
#' @param subjectid_var character. Name of the subject identifier variable in
#'   the data (default is "USUBJID").
#' @param dat data.table. The analysis data set.
#'
#' @return An integer
#' @export
#' 
n_sub_ <-
  function(dat,
           cell_index,
           subjectid_var) {
    dat[list(cell_index)] |>
      data.table::uniqueN(by = c(subjectid_var))
  }

#' Building-block: Number of events (multiple events counted multiple times)
#'
#' @param dat data.table. The analysis data set.
#' @param intersect_index A vector of intergers referencing the rows of `dat`
#'   that match both (1) the population to be analyzed and (2) the
#'   defenition of an event
#'
#' @return an integer value
#' @export
#'
n_event_ <- function(dat, intersect_index) {
  dat[list(intersect_index)] |>
    NROW()
}


#' Building-block: Number of subjects with at least one event
#'
#' @param dat data.table. The analysis data set.
#' @param intersect_index A vector of intergers referencing the rows of `dat`
#'   that match both (1) the population to be analyzed and (2) the defenition of
#'   an event
#' @param subjectid_var character. Name of the subject identifier variable in
#'   the data (default is "USUBJID").
#'
#' @return an interger value
#' @export
#'
n_subj_event_ <- function(dat, intersect_index, subjectid_var) {
  dat[list(intersect_index)] |>
    data.table::uniqueN(by = subjectid_var)
}

#' Building-block: Proportion of subjects having at least one event
#'
#' @param dat data.table. The analysis data set.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog} for more
#'   information.
#' @param intersect_index A vector of intergers referencing the rows of `dat`
#'   that match both (1) the population to be analyzed and (2) the defenition of
#'   an event
#' @param subjectid_var character. Name of the subject identifier variable in
#'   the data (default is "USUBJID").
#'
#' @return an integer value
#' @export 
p_subj_event_ <-
  function(dat,
           cell_index,
           intersect_index,
           subjectid_var) {
    n_sub <- n_sub_(dat, cell_index = cell_index,subjectid_var = subjectid_var)
    
    # If there are no subjects, no need for further calculations
    if (n_sub == 0) {
      return(NaN)
    }
    
    # Filter analysis data to cell specific content
    n_subj_event <-
      n_subj_event_(dat, intersect_index = intersect_index, subjectid_var = subjectid_var)
    
    n_subj_event / n_sub * 100
  }
