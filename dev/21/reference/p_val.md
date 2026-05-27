# Wrapper for P-value calculations

Ensures the input to the statistical function is in the proper format,
and enruse the output is formated to the need of the AMNOG workflow.

## Usage

``` r
p_val(
  dat,
  event_index,
  cell_index,
  treatment_var,
  treatment_refval,
  subjectid_var,
  safe_mode = FALSE,
  threshold_lower = 5,
  threshold_upper = 200,
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

- safe_mode:

  Boolean determing if the function should fail when given input that
  cannot be calculated (`safe_mode = TRUE`), or if it should silently
  return a `NA` value (default).

- threshold_lower:

  numeric. Lower threshold limit for selecting Fischer vs. Barnard.

- threshold_upper:

  numeric. Upper threshold limit for selecting Fischer vs. Barnard.

- ...:

  Optional parameters.

## Value

A data.table containing p-value statistics.

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
p_val(dat, event_index = c(1L, 2L, 4L), cell_index = dat[["INDEX_"]],
      treatment_var = "TRT", treatment_refval = "Placebo",
      subjectid_var = "USUBJID")
#> ----- Calculating Barnards test - this may take a minute 
#>      label    description qualifiers  value
#>     <char>         <char>     <char>  <num>
#> 1: p-value Barnard's test       <NA> 0.6875
```
