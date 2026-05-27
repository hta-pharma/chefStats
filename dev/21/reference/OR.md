# Wrapper for Odds Ratio calculation

Ensures the input to the statistical function is in the proper format,
and ensures the output is formatted to the need of the AMNOG workflow.

## Usage

``` r
OR(
  dat,
  event_index,
  cell_index,
  treatment_var,
  treatment_refval,
  subjectid_var,
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

- cell_index:

  A vector of integers referencing the rows of `dat` (as specified by
  the `INDEX_` column in `dat`) that match the population to be
  analyzed. See the "Endpoint Events" vignette in ramnog for more
  information.

- treatment_var:

  character. The name of the treatment variable in the data.

- treatment_refval:

  character. The reference value of the treatment variable in the data.

- subjectid_var:

  character. Name of the subject identifier variable in the data
  (default is "USUBJID").

- ...:

  Optional parameters.

## Value

A data.table containing Odds Ratio statistics.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
  TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
)
dat[, INDEX_ := .I]
#>    USUBJID     TRT INDEX_
#>     <char>  <char>  <int>
#> 1:      S1  Active      1
#> 2:      S2  Active      2
#> 3:      S3  Active      3
#> 4:      S4 Placebo      4
#> 5:      S5 Placebo      5
#> 6:      S6 Placebo      6
data.table::setkey(dat, INDEX_)
OR(dat, event_index = c(1L, 2L, 4L), cell_index = dat[["INDEX_"]],
   treatment_var = "TRT", treatment_refval = "Placebo",
   subjectid_var = "USUBJID")
#>     label                   description qualifiers       value
#>    <char>                        <char>     <char>       <num>
#> 1:     OR                    Odds Ratio       <NA> 0.250000000
#> 2:     SE     Odds Ratio standard error       <NA> 1.732050808
#> 3:   ORUL Odds Ratio 95%-CI upper limit       <NA> 7.451853856
#> 4:   ORLL Odds Ratio 95%-CI lower limit       <NA> 0.008387175
```
