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
# make_two_by_two_ puts the non-reference treatment in row 1 and the reference
# (here "Placebo") in row 2. We sort decreasing to match that order
# ("Xanomeline High Dose" first, then "Placebo").
no_counts <- no_events[!(event),.N,by=TRT01A][order(TRT01A, decreasing = TRUE)]

events <- at_risk[AEDECOD=="PRURITUS"] |> unique(by="USUBJID")
yes_counts <- events[,.N,by=TRT01A][order(TRT01A, decreasing = TRUE)]

expected <- as.matrix(cbind(yes_counts[,.(N)], no_counts[,.(N)]))

rownames(expected) <- no_counts$TRT01A
colnames(expected) <- c("outcome_YES", "outcome_NO")

# With data.table 1.16, attributes are preserved in a different way, so strip
# these for testing.
attr(dimnames(expected)[[1]], "label") <- NULL

expect_identical(actual, expected)
})


# Regression tests for row-order convention ------------------------------------
# These tests pin down the contract that make_two_by_two_() returns the
# non-reference treatment in row 1 and the reference in row 2, and that the
# downstream effect estimators (RR, OR, risk_diff) follow the conventional
# direction: risk/odds of (non-reference) over (reference).

test_that("make_two_by_two_ puts non-reference treatment in row 1", {
  dat <- data.table::data.table(
    USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
    TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
  )
  dat[, INDEX_ := .I]
  data.table::setkey(dat, INDEX_)

  # 2 events in Active (S1, S2), 1 event in Placebo (S4)
  tt <- make_two_by_two_(
    dat,
    event_index      = c(1L, 2L, 4L),
    cell_index       = dat$INDEX_,
    treatment_var    = "TRT",
    treatment_refval = "Placebo",
    subjectid_var    = "USUBJID"
  )

  expect_identical(rownames(tt), c("Active", "Placebo"))
  expect_equal(tt["Active",  "outcome_YES"], 2)
  expect_equal(tt["Active",  "outcome_NO"],  1)
  expect_equal(tt["Placebo", "outcome_YES"], 1)
  expect_equal(tt["Placebo", "outcome_NO"],  2)
})

test_that("RR is risk(non-reference) / risk(reference)", {
  dat <- data.table::data.table(
    USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
    TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
  )
  dat[, INDEX_ := .I]
  data.table::setkey(dat, INDEX_)

  out <- RR(
    dat,
    event_index      = c(1L, 2L, 4L),
    cell_index       = dat$INDEX_,
    treatment_var    = "TRT",
    treatment_refval = "Placebo",
    subjectid_var    = "USUBJID"
  )

  # risk(Active)  = 2/3
  # risk(Placebo) = 1/3
  # RR = (2/3) / (1/3) = 2
  expect_equal(out[label == "RR", value], 2, tolerance = 1e-8)
})

test_that("OR is odds(non-reference) / odds(reference)", {
  dat <- data.table::data.table(
    USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
    TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
  )
  dat[, INDEX_ := .I]
  data.table::setkey(dat, INDEX_)

  out <- OR(
    dat,
    event_index      = c(1L, 2L, 4L),
    cell_index       = dat$INDEX_,
    treatment_var    = "TRT",
    treatment_refval = "Placebo",
    subjectid_var    = "USUBJID"
  )

  # odds(Active)  = 2/1
  # odds(Placebo) = 1/2
  # OR = 2 / 0.5 = 4
  expect_equal(out[label == "OR", value], 4, tolerance = 1e-8)
})

test_that("risk difference is risk(non-reference) - risk(reference)", {
  dat <- data.table::data.table(
    USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
    TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
  )
  dat[, INDEX_ := .I]
  data.table::setkey(dat, INDEX_)

  out <- RD(
    dat,
    event_index      = c(1L, 2L, 4L),
    cell_index       = dat$INDEX_,
    treatment_var    = "TRT",
    treatment_refval = "Placebo",
    subjectid_var    = "USUBJID"
  )

  # RD = (2/3 - 1/3) * 100 = 33.333...
  expect_equal(out[label == "RD", value], (2/3 - 1/3) * 100, tolerance = 1e-8)
})
