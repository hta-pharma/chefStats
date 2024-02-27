mk_adcm <- function(study_metadata){
  adcm <- data.table::as.data.table(pharmaverseadam::adcm)
  adsl <- data.table::as.data.table(pharmaverseadam::adsl)
  adcm_out <-
    merge(adsl, adcm[, c(setdiff(names(adcm), names(adsl)), "USUBJID"), with =
                       F], by = "USUBJID", all = TRUE)
  adcm_out[]
}

mk_adae <- function(study_metadata){
  adae <- data.table::as.data.table(pharmaverseadam::adae)
  adsl <- data.table::as.data.table(pharmaverseadam::adsl)

  adsl <- adsl[TRT01A %in% c('Placebo', 'Xanomeline High Dose')]
  adae <- adae[TRT01A %in% c('Placebo', 'Xanomeline High Dose')]
  adae_out <-
    merge(adsl, adae[, c(setdiff(names(adae), names(adsl)), "USUBJID"), with =
                       F], by = "USUBJID", all = TRUE)

}


mk_advs <- function(study_metadata) {

  # Read ADSL
  adsl <- data.table::as.data.table(pharmaverseadam::adsl)

  # Filter treatment arms
  adsl <- adsl[adsl$TRT01A %in% c('Placebo', 'Xanomeline High Dose')]
  adsl[1,AGEGR1:=NA_character_]
  adsl[2:10,SEX:=NA_character_]

  # Read ADVS
  advs <- data.table::as.data.table(pharmaverseadam::advs)

  # Identify baseline body weight
  advs_bw <- advs[advs$PARAMCD == "WEIGHT" & advs$VISIT == "BASELINE"]

  # Create new variable BW_BASELINE
  advs_bw[["BW_BASELINE"]] <- advs_bw[["AVAL"]]

  # Merge ADSL, ADAE and baseline body weight from ADVS
  adam_out <-
    merge(adsl, advs_bw[, c("BW_BASELINE", "USUBJID")], by = "USUBJID", all.x = TRUE)

  return(adam_out)
}


mk_adlb <- function(study_metadata) {
  # Read ADSL
  adsl <- data.table::as.data.table(pharmaverseadam::adsl)

  # Filter treatment arms
  adsl <-
    adsl[adsl$TRT01A %in% c('Placebo', 'Xanomeline High Dose')]


  # Read ADLB
  adlb <- data.table::as.data.table(pharmaverseadam::adlb) %>%
    .[.[["PARAMCD"]] == 'SODIUM' &
        .[["AVISIT"]] %in% c("Baseline", "Week 8", "Week 16"), ]

  adlb2 <-
    merge(adlb,
          adlb[adlb$AVISIT == "Baseline", c("USUBJID", "AVAL")],
          by = "USUBJID", all.x = TRUE)

  adlb2[["VALUE_BASELINE"]] <- adlb2[["AVAL.y"]]
  adlb2[["VALUE_CHANGE"]] <- adlb2[["AVAL.x"]] - adlb2[["AVAL.y"]]

  # Merge ADSL and ADLB
  adam_out <-
    merge(adsl, adlb2[, c("USUBJID",
                          "PARAMCD",
                          "AVISIT",
                          "VALUE_BASELINE",
                          "VALUE_CHANGE",
                          "ANL01FL")], by = "USUBJID", all.x = TRUE)

  return(adam_out)
}
