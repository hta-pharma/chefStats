# Calculate percentage of subjects with events

Calculate the percentage of subjects with events by treatment and
strata.

## Usage

``` r
p_subj_event_by_trt(
  dat,
  event_index,
  cell_index,
  subjectid_var,
  treatment_var,
  treatment_value,
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

- subjectid_var:

  character. Name of the subject identifier variable in the data
  (default is "USUBJID").

- treatment_var:

  character. Name of the treatment variable in the data.

- treatment_value:

  character. Value of the treatment variable in the data.

- ...:

  Optional parameters.

## Value

A data.table containing the percentage of subjects with events by
treatment.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4"),
  TRT     = c("Active", "Active", "Placebo", "Placebo")
)
dat[, INDEX_ := .I]
#>    USUBJID     TRT INDEX_
#>     <char>  <char>  <int>
#> 1:      S1  Active      1
#> 2:      S2  Active      2
#> 3:      S3 Placebo      3
#> 4:      S4 Placebo      4
data.table::setkey(dat, INDEX_)
p_subj_event_by_trt(dat, event_index = c(1L, 2L),
                    cell_index = dat[TRT == "Active"][["INDEX_"]],
                    subjectid_var = "USUBJID",
                    treatment_var = "TRT", treatment_value = "Active")
#>                           description qualifiers  label value
#>                                <char>     <char> <char> <num>
#> 1: Proportion of subjects with events       <NA>    (%)   100
```
