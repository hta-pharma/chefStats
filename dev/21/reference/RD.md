# Wrapper for Risk Difference calculation

Ensures the input to the statistical function is in the proper format,
and ensures the output is formatted to the need of the AMNOG workflow.

## Usage

``` r
RD(
  dat,
  event_index,
  cell_index,
  treatment_var,
  treatment_refval,
  subjectid_var,
  as_pct = TRUE,
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

- as_pct:

  Boolean.

- ...:

  Optional parameters.

## Value

A data.table containing Risk Difference statistics.

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
RD(dat, event_index = c(1L, 2L, 4L), cell_index = dat[["INDEX_"]],
   treatment_var = "TRT", treatment_refval = "Placebo",
   subjectid_var = "USUBJID")
#>     label                        description qualifiers       value
#>    <char>                             <char>     <char>       <num>
#> 1:     RD                    Risk Difference       <NA>  33.3333333
#> 2:     SE     Risk Difference standard error       <NA>   0.3849002
#> 3:   RDUL Risk Difference 95%-CI upper limit       <NA> 108.7723823
#> 4:   RDLL Risk Difference 95%-CI lower limit       <NA> -42.1057156
```
