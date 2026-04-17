#' Calculate number of subjects by treatment and stratum
#'
#' @description Calculate the number of subjects by treatment and stratum.
#'
#' @param dat data.table. The analysis data set.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#'
#' @return A data.table containing the number of subjects for the given combination of treatment and stratum.
#' @export
#' @import data.table
#' @examples
#' dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3"))
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' n_subj(dat, cell_index = dat[["INDEX_"]], subjectid_var = "USUBJID")
n_subj <- function(dat,
                   cell_index,
                   subjectid_var,
                   ...) {
  stat <-
    n_subj_(dat = dat,
           cell_index = cell_index,
           subjectid_var = subjectid_var)
  
  out <- data.table(
    description = "Number of subjects",
    qualifiers = NA_character_,
    label = "N",
    value = as.double(stat)
  )
  out[]
}


#' Calculate number of events by treatment and stratum
#'
#' @description Calculate the number of events by treatment and stratum.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#'
#' @return A data.table containing the number of events for the given combination of treatment and stratum.
#' @export
#' @examples
#' dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3", "S4"))
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' n_event(dat, event_index = c(1L, 2L), cell_index = dat[["INDEX_"]],
#'         subjectid_var = "USUBJID")
n_event <-
  function(dat,
           event_index,
           cell_index,
           subjectid_var,
           ...) {
    intersect_index <- intersect(event_index, cell_index)
    stat <- n_event_(dat = dat, intersect_index = intersect_index)
    
    return(
      data.table::data.table(
        description = "Number of events",
        qualifiers = NA_character_,
        label = "E",
        value = as.double(stat)
      )
    )
    
  }


#' Calculate number of subjects with events by treatment and stratum
#'
#' @description Calculate the number of subjects with events by treatment and stratum.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#'
#' @return A data.table containing the number of subjects with events for the given combination of treatment and stratum.
#' @export
#' @examples
#' dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3", "S4"))
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' n_subj_event(dat, event_index = c(1L, 2L), cell_index = dat[["INDEX_"]],
#'              subjectid_var = "USUBJID")
n_subj_event <-
  function(dat,
           event_index,
           cell_index,
           subjectid_var,
           ...) {
    intersect_index <- intersect(event_index, cell_index)
    stat <-
      n_subj_event_(dat = dat,
                    intersect_index = intersect_index,
                    subjectid_var = subjectid_var)
    
    return(
      data.table::data.table(
        description = "Number of subjects with events",
        qualifiers = NA_character_,
        label = "n",
        value = as.double(stat)
      )
    )
  }


#' Calculate percentage of subjects with events by treatment and stratum
#'
#' @description Calculate the percentage of subjects with events by treatment and stratum.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#'
#' @return A data.table containing the percentage of subjects with events for the given combination of treatment and stratum
#' @export
#' @examples
#' dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3", "S4"))
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' p_subj_event(dat, event_index = c(1L, 2L), cell_index = dat[["INDEX_"]],
#'              subjectid_var = "USUBJID")
p_subj_event <-
  function(dat,
           event_index,
           cell_index,
           subjectid_var,
           ...) {
    intersect_index <- intersect(event_index, cell_index)
    stat <-
      p_subj_event_(
        dat = dat,
        cell_index = cell_index,
        intersect_index = intersect_index,
        subjectid_var = subjectid_var
      )
    
    return(
      data.table::data.table(
        description = "Proportion of subjects with events",
        qualifiers = NA_character_,
        label = "(%)",
        value = stat
      )
    )
  }



#' @title Produce counts of Number of subjects, number of events, number of
#'   subjects with events, and proportion of subjects with events
#'
#' @description A short cut - instead of calling the individual function
#'   (`n_sub`, `n_event`, `n_subj_event`, `p_subj_event`), one call to this
#'   function will produce all the described functions. This can be useful to
#'   save compute time as inside the chef pipeline there will be fewer
#'   iterations.
#'
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#' @return a data.table containing all statistical outputs
#' @export
#' @examples
#' dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3", "S4"))
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' count_set(dat, event_index = c(1L, 2L), cell_index = dat[["INDEX_"]],
#'           subjectid_var = "USUBJID")
count_set <- function(dat,
                   event_index,
                   cell_index,
                   subjectid_var,
                   ...) {
  intersect_index <- intersect(event_index, cell_index)
  n_subjects <-
    n_subj_(dat = dat,
           cell_index = cell_index,
           subjectid_var = subjectid_var)
  n_events <- n_event_(dat = dat, intersect_index = intersect_index)
  n_subjects_with_event <-
    n_subj_event_(dat = dat,
                  intersect_index = intersect_index,
                  subjectid_var = subjectid_var)
  proportion_subjects_with_event <-
    p_subj_event_(
      dat = dat,
      cell_index = cell_index,
      intersect_index = intersect_index,
      subjectid_var = subjectid_var
    )
  
  description_vec <- c("Number of subjects", "Number of events", "Number of subjects with events", "Proportion of subjects with events")
  label_vec <- c("N","E", "n", "(%)")
  stat_vec <- c(n_subjects, n_events, n_subjects_with_event, proportion_subjects_with_event)
  
  return(
    data.table::data.table(
      description = description_vec,
      qualifiers = NA_character_,
      label = label_vec,
      value = stat_vec
    )
  )
}


#' Calculate summary statistics for demographics on a continuous variable
#'
#'@description Calculate a set of summary statistics (mean, median, sd, min, max, n_non_missing, n_missing) on a continuous variable for demographics endpoints.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param var character. Name of the variable in the analysis data that is subject to the statistics.
#' @param ... Optional parameters.
#'
#' @return A data.table containing the summary statistics for the given continuous variable in the analysis data set.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID = c("S1", "S2", "S3", "S4"),
#'   AGE     = c(45, 52, 60, 38)
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' demographics_continuous(dat, event_index = dat[["INDEX_"]],
#'                         cell_index = dat[["INDEX_"]],
#'                         subjectid_var = "USUBJID", var = "AGE")
demographics_continuous <- function(dat,
                                    event_index,
                                    cell_index,
                                    subjectid_var,
                                    var,
                                    ...) {
  # Filter analysis data to cell specific content
  intersect_index <- intersect(cell_index, event_index)
  dat_cell <- dat[list(intersect_index)] |>
    unique(by = c(subjectid_var))
  
  # Return statistics depending on the type of variable (continuous or categorical)
  stat <- dat_cell[,
                   .(
                     mean = mean(get(var), na.rm = TRUE),
                     median = stats::median(get(var), na.rm = TRUE),
                     sd = stats::sd(get(var), na.rm = TRUE),
                     min = min(get(var), na.rm = TRUE),
                     max = max(get(var), na.rm = TRUE),
                     n_non_missing = sum(!is.na(get(var))),
                     n_missing = sum(is.na(get(var)))
                   )]
  return(
    data.table::data.table(
      label = names(stat),
      description = "Demographics",
      qualifiers = var,
      value = as.double(unlist(stat[1, .SD]))
    )
  )
  
}


#' Calculate basic summary statistics for demographics
#'
#'@description Calculate summary statistics (n_non_missing, n_missing) on a variable for demographics endpoints.
#'
#' @param dat data.table. The analysis data set.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param stratify_by character vector. Set of variables in the analysis data to stratify by.
#' @param strata_var character. Variable in the analysis data to stratify by specific for this call.
#' @param ... Optional parameters.
#'
#' @return A data.table containing the summary statistics.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID = c("S1", "S2", "S3", "S4"),
#'   SEX     = c("M", "F", "M", NA_character_)
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' demographics_counts(dat, cell_index = dat[["INDEX_"]],
#'                     subjectid_var = "USUBJID",
#'                     stratify_by = c("TOTAL_", "SEX"),
#'                     strata_var = "SEX")
demographics_counts <- function(dat,
                                cell_index,
                                subjectid_var,
                                stratify_by,
                                strata_var,
                                ...) {
  # Filter analysis data to cell specific content
  dat_cell <- dat[list(cell_index)] |>
    unique(by = c(subjectid_var))
  
  # Return statistics depending on the type of variable (continuous or categorical)
  if (strata_var == "TOTAL_") {
    return(total_missing_counts(dat_cell, stratify_by))
  }
  stat <-
    dat_cell[, .(n_non_missing = sum(!is.na(get(strata_var))))]
  return(
    data.table::data.table(
      label = names(stat),
      description = paste0("Demographics"),
      qualifiers = NA_character_,
      value = as.double(unlist(stat[1, .SD]))
    )
  )
}

#' Count missing and non-missing
#'
#' @description A helper function for `demographics()`
#' @param dat_cell The data_cell produced in `demographics()`
#' @param stratify_by The `stratify_by` column in the endpoint specification
#'   data object made in `chef`
#' @param ... Optional parameters.
#'
#' @return A data.table containing the summary statistics.
#' @noRd
total_missing_counts <- function(dat_cell, stratify_by) {
  stratify_by_subset <- setdiff(stratify_by, "TOTAL_")
  stat <- lapply(stratify_by_subset, function(strata_i) {
    stat <- dat_cell[, .(n_non_missing = sum(!is.na(get(strata_i))),
                         n_missing = sum(is.na(get(strata_i))))]
  })
  value <- NULL
  
  out <- data.table::rbindlist(stat) |>
    data.table::transpose(keep.names = "label") |>
    data.table::setnames(new = c("label", stratify_by_subset)) |>
    data.table::melt.data.table(measure.vars = stratify_by_subset,
                                variable.name = "qualifiers")
  out[, `:=`(value = as.double(value), description = "Demographics")]
  out[]
}

#' Calculate percentage of subjects with events
#'
#' @description Calculate the percentage of subjects with events by treatment and strata.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param treatment_var character. Name of the treatment variable in the data.
#' @param treatment_value character. Value of the treatment variable in the data.
#' @param ... Optional parameters.
#'
#' @return A data.table containing the percentage of subjects with events by treatment.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID = c("S1", "S2", "S3", "S4"),
#'   TRT     = c("Active", "Active", "Placebo", "Placebo")
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' p_subj_event_by_trt(dat, event_index = c(1L, 2L),
#'                     cell_index = dat[TRT == "Active"][["INDEX_"]],
#'                     subjectid_var = "USUBJID",
#'                     treatment_var = "TRT", treatment_value = "Active")
p_subj_event_by_trt <-
  function(dat,
           event_index,
           cell_index,
           subjectid_var,
           treatment_var,
           treatment_value,
           ...) {
    n_sub <- dat[dat[[treatment_var]] == treatment_value] |>
      uniqueN(by = c(subjectid_var))
    
    if (n_sub == 0) {
      return(
        data.table(
          description = "Proportion of subjects with events",
          qualifiers = NA_character_,
          label = "(%)",
          value = NaN
        )
      )
    }
    
    intersect_index <- intersect(cell_index, event_index)
    n_subev <- dat[list(intersect_index)] |>
      uniqueN(by = c(subjectid_var))
    
    out <-
      data.table(
        description = "Proportion of subjects with events",
        qualifiers = NA_character_,
        label = "(%)",
        value = n_subev / n_sub * 100
      )
    
    return(out)
  }

#' Calculate observation time by treatment
#'
#' @param dat data.table. The analysis data set.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param ... Optional parameters.
#'
#' @return A data.table containing the observation time by treatment.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID  = c("S1", "S2", "S3"),
#'   INTRDURY = c(1.5, 2.0, 0.8)
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' obs_time_by_trt(dat, cell_index = dat[["INDEX_"]], subjectid_var = "USUBJID")
obs_time_by_trt <- function(dat,
                            cell_index,
                            subjectid_var,
                            ...) {
  obs_time <- dat[J(cell_index)] |>
    unique(by = c(subjectid_var)) |>
    with(sum(INTRDURY, na.rm = TRUE))
  
  out <-
    data.table(
      description = "Observation time (years)",
      qualifiers = NA_character_,
      label = "Obs. time",
      value = round(obs_time)
    )
  
  return(out)
}

#' Calculate number of events per 100 years of exposure
#'

#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param treatment_var character. Name of the treatment variable in the data.
#' @param treatment_value character. Value of the treatment variable in the data.
#' @param ... Optional parameters.
#'
#' @return A data.table containing the number of events per 100 years of exposure.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID  = c("S1", "S2", "S3", "S4"),
#'   TRT      = c("Active", "Active", "Placebo", "Placebo"),
#'   INTRDURY = c(1.5, 2.0, 0.8, 1.2)
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' n_event_100y(dat, event_index = c(1L, 2L),
#'              cell_index = dat[TRT == "Active"][["INDEX_"]],
#'              subjectid_var = "USUBJID",
#'              treatment_var = "TRT", treatment_value = "Active")
n_event_100y <- function(dat,
                         event_index,
                         cell_index,
                         subjectid_var,
                         treatment_var,
                         treatment_value,
                         ...) {
  # Observation time (years) in treatment arm
  obs_time <- obs_time_by_trt(dat = dat,
                              cell_index = dat[["INDEX_"]][which(dat[[treatment_var]] == treatment_value)],
                              subjectid_var = subjectid_var,)[["value"]]
  
  # Number of events
  intersect_index <- intersect(event_index, cell_index)
  n_event <- dat[list(intersect_index)] |>
    NROW()
  
  out <-
    data.table(
      description = "Events per 100 years of exposure",
      qualifiers = NA_character_,
      label = "R",
      value = round(n_event / obs_time * 100)
    )
  
  return(out)
}

#' Calculate mean value
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param var character. Name of the variable in the analysis data that is subject to the statistics.
#' @param ... Optional parameters.
#'
#' @return A data.table containing the mean value.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID = c("S1", "S2", "S3"),
#'   AVAL    = c(1.5, 2.5, 3.5)
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' mean_value(dat, event_index = dat[["INDEX_"]], cell_index = dat[["INDEX_"]],
#'            subjectid_var = "USUBJID", var = "AVAL")
mean_value <- function(dat,
                       event_index,
                       cell_index,
                       subjectid_var,
                       var,
                       ...) {
  # Filter analysis data to cell specific content
  intersect_index <- intersect(cell_index, event_index)
  dat_cell <- dat[list(intersect_index)] |>
    unique(by = c(subjectid_var))
  
  stat <- dat_cell[[var]] |>
    mean()
  
  return(data.table(
    label = "mean",
    description = "Mean value",
    qualifiers = var,
    value = stat
  ))
}

#' Standard deviation
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in \pkg{ramnog}
#'   for more information.
#' @param subjectid_var character. Name of the subject identifier variable in the data (default is "USUBJID").
#' @param var Character. Name of the variable in the analysis data that is subject to the statistics.
#' @param ... Optional parameters.
#'
#' @return A data.table containing the standard deviation.
#' @export
#' @examples
#' dat <- data.table::data.table(
#'   USUBJID = c("S1", "S2", "S3"),
#'   AVAL    = c(1.5, 2.5, 3.5)
#' )
#' dat[, INDEX_ := .I]
#' data.table::setkey(dat, INDEX_)
#' sd_value(dat, event_index = dat[["INDEX_"]], cell_index = dat[["INDEX_"]],
#'          subjectid_var = "USUBJID", var = "AVAL")
sd_value <- function(dat,
                     event_index,
                     cell_index,
                     subjectid_var,
                     var,
                     ...) {
  intersect_index <- intersect(cell_index, event_index)
  dat_cell <- dat[J(intersect_index)] |>
    unique(by = c(subjectid_var))
  
  stat <- dat_cell[[var]] |>
    stats::sd()
  
  return(
    data.table(
      label = "SD",
      description = "Standard deviation",
      qualifiers = var,
      value = stat
    )
  )
}
