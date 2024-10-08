test_that("make_two_by_two_ adds empty levels in 2x2 table", {

# SETUP -------------------------------------------------------------------
  input <- mk_adae()
  input[, INDEX_ := .I] |> setkey(INDEX_)
  cell_index <-
    input[SAFFL == "Y" & SEX == "F"][["INDEX_"]]
  event_index <-
    input[AEDECOD == "PRURITUS"][["INDEX_"]]


# ACT ---------------------------------------------------------------------
  actual <- make_two_by_two_(
      dat = input,
      event_index = event_index,
      cell_index = cell_index,
      treatment_var = "TRT01A",
      treatment_refval = "Placebo",
      subjectid_var = "USUBJID"
    )


# EXPECT ------------------------------------------------------------------
at_risk <- input[SAFFL == "Y" & SEX == "F"]
  at_risk[,event:=FALSE]
  at_risk[AEDECOD=="PRURITUS",event:=TRUE]
at_risk  |> setorder(USUBJID, -event)

no_events <- at_risk |> unique(by="USUBJID")
no_counts <- no_events[!(event),.N,by=TRT01A][order(TRT01A)]

events <- at_risk[AEDECOD=="PRURITUS"] |> unique(by="USUBJID")
yes_counts <- events[,.N,by=TRT01A][order(TRT01A)]

expected <- as.matrix(cbind(yes_counts[,.(N)], no_counts[,.(N)]))

rownames(expected) <- no_counts$TRT01A
colnames(expected) <- c("outcome_YES", "outcome_NO")

# With data.table 1.16, attributes are preserved in a different way, so strip
# these for testing.
attr(dimnames(expected)[[1]], "label") <- NULL

expect_identical(actual, expected)
})
