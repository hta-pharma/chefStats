# Calculate number of subjects by treatment and stratum

Calculate the number of subjects by treatment and stratum.

## Usage

``` r
n_subj(dat, cell_index, subjectid_var, ...)
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

A data.table containing the number of subjects for the given combination
of treatment and stratum.

## Examples

``` r
dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3"))
dat[, INDEX_ := .I]
#>    USUBJID INDEX_
#>     <char>  <int>
#> 1:      S1      1
#> 2:      S2      2
#> 3:      S3      3
data.table::setkey(dat, INDEX_)
n_subj(dat, cell_index = dat[["INDEX_"]], subjectid_var = "USUBJID")
#>           description qualifiers  label value
#>                <char>     <char> <char> <num>
#> 1: Number of subjects       <NA>      N     3
```
