setup_basic_counts <- function() {
  SAFFL <- SEX <- TRT01A <- AEDECOD <- NULL
  input <- mk_adae(study_metadata = NULL)
  input[, INDEX_ := .I] |> setkey(INDEX_)
  cell_index_f <-
    input[SAFFL == "Y" &
            TRT01A == "Placebo" & SEX == "F"][["INDEX_"]]
  cell_index_total <-
    input[SAFFL == "Y" & TRT01A == "Placebo"][["INDEX_"]]
  event_index <-
    input[AEDECOD == "ERYTHEMA"][["INDEX_"]]
  return(list(
    input = input,
    cell_index_f = cell_index_f,
    cell_index_total = cell_index_total,
    event_index = event_index
  ))
}


setup_by_strata_across_trt <- function(){
  SAFFL <- SEX <- TRT01A <- NULL
  input <- mk_adae(study_metadata = NULL)
  input[, INDEX_ := .I] |> data.table::setkey(INDEX_)
  cell_index_f <-
    input[SAFFL == "Y" &
            (TRT01A == "Placebo" | TRT01A == "Xanomeline High Dose") & SEX == "F"][["INDEX_"]]
  cell_index_total <-
    input[SAFFL == "Y" & (TRT01A == "Placebo" | TRT01A == "Xanomeline High Dose")][["INDEX_"]]
  event_index <-
    input[AEDECOD == "ERYTHEMA"][["INDEX_"]]
  
  return(list(
    input = input,
    cell_index_f = cell_index_f,
    cell_index_total = cell_index_total,
    event_index = event_index
  ))
}
