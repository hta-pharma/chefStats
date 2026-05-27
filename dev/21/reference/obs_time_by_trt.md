# Calculate observation time by treatment

Calculate observation time by treatment

## Usage

``` r
obs_time_by_trt(dat, cell_index, subjectid_var, ...)
```

## Arguments

- dat:

  data.table. The analysis data set.

- cell_index:

  A vector of integers referencing the rows of `dat` (as specified by
  the `INDEX_` column in `dat`) that match the population to be
  analyzed. See the "Endpoint Events" vignette in ramnog for more
  information.

- subjectid_var:

  character. Name of the subject identifier variable in the data
  (default is "USUBJID").

- ...:

  Optional parameters.

## Value

A data.table containing the observation time by treatment.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID  = c("S1", "S2", "S3"),
  INTRDURY = c(1.5, 2.0, 0.8)
)
dat[, INDEX_ := .I]
#>    USUBJID INTRDURY INDEX_
#>     <char>    <num>  <int>
#> 1:      S1      1.5      1
#> 2:      S2      2.0      2
#> 3:      S3      0.8      3
data.table::setkey(dat, INDEX_)
obs_time_by_trt(dat, cell_index = dat[["INDEX_"]], subjectid_var = "USUBJID")
#>                 description qualifiers     label value
#>                      <char>     <char>    <char> <num>
#> 1: Observation time (years)       <NA> Obs. time     4
```
