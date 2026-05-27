# Wrapper for P-value of interaction tests

Wrapper for P-value of interaction tests

## Usage

``` r
p_val_interaction(
  dat,
  event_index,
  treatment_var,
  treatment_refval,
  subjectid_var,
  strata_var,
  odds_ratio = NA,
  correct = FALSE,
  ...
)
```

## Arguments

- dat:

  data.table. The analysis data set.

- event_index:

  vector of integers that index the rows in `dat` that match the
  definition of an 'event'. Matching is done via the `INDEX_` column in
  `dat`.

- treatment_var:

  character. The name of the treatment variable in the data.

- treatment_refval:

  character. The reference value of the treatment variable in the data.

- subjectid_var:

  character. Name of the subject identifier variable in the data
  (default is "USUBJID").

- strata_var:

  character. Variable in the analysis data to stratify by specific for
  this call.

- odds_ratio:

  numeric. Odds Ration (default = NA).

- correct:

  logical. If TRUE Tarones correction is returned (default = FALSE).

- ...:

  Optional parameters.

## Value

A data.table containing the statistics for p-value interaction tests.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
  TRT     = c("Active", "Active", "Active", "Active",
              "Placebo", "Placebo", "Placebo", "Placebo"),
  STRATA  = c("M", "M", "F", "F", "M", "M", "F", "F")
)
dat[, INDEX_ := .I]
#>    USUBJID     TRT STRATA INDEX_
#>     <char>  <char> <char>  <int>
#> 1:      S1  Active      M      1
#> 2:      S2  Active      M      2
#> 3:      S3  Active      F      3
#> 4:      S4  Active      F      4
#> 5:      S5 Placebo      M      5
#> 6:      S6 Placebo      M      6
#> 7:      S7 Placebo      F      7
#> 8:      S8 Placebo      F      8
data.table::setkey(dat, INDEX_)
p_val_interaction(dat, event_index = c(1L, 3L, 5L, 7L),
                  treatment_var = "TRT", treatment_refval = "Placebo",
                  subjectid_var = "USUBJID", strata_var = "STRATA")
#>                  label                                    description
#>                 <char>                                         <char>
#> 1: P-value interaction Breslow-Day test on Homogeneity of Odds Ratios
#>    qualifiers value
#>        <char> <num>
#> 1:       <NA>     1
```
