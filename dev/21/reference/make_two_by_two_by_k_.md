# Make 2x2xk contingency tables from summarized adam data. This function is NOT generalized

Make 2x2xk contingency tables from summarized adam data. This function
is NOT generalized

## Usage

``` r
make_two_by_two_by_k_(
  dat,
  event_index,
  strata_var,
  treatment_var,
  treatment_refval,
  subjectid_var
)
```

## Arguments

- dat:

  data.table. The analysis data set.

- event_index:

  vector of integers that index the rows in `dat` that match the
  definition of an 'event'. Matching is done via the `INDEX_` column in
  `dat`.

- strata_var:

  character. Variable in `dat` to stratify by specific for this call.

- treatment_var:

  character. The name of the treatment variable in `dat`.

- treatment_refval:

  character. The reference value of the treatment variable in `dat`.

- subjectid_var:

  character. Name of the subject identifier variable in `dat` (default
  is "USUBJID").

## Value

A two-by-two-by-k array where k represents the number of subgroups
(strata).

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
make_two_by_two_by_k_(dat, event_index = c(1L, 3L, 5L, 7L),
                       strata_var = "STRATA",
                       treatment_var = "TRT", treatment_refval = "Placebo",
                       subjectid_var = "USUBJID")
#> , , M
#> 
#>         outcome_YES outcome_NO
#> Placebo           1          1
#> Active            1          1
#> 
#> , , F
#> 
#>         outcome_YES outcome_NO
#> Placebo           1          1
#> Active            1          1
#> 
```
