# Calculate basic summary statistics for demographics

Calculate summary statistics (n_non_missing, n_missing) on a variable
for demographics endpoints.

## Usage

``` r
demographics_counts(
  dat,
  cell_index,
  subjectid_var,
  stratify_by,
  strata_var,
  ...
)
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

- stratify_by:

  character vector. Set of variables in the analysis data to stratify
  by.

- strata_var:

  character. Variable in the analysis data to stratify by specific for
  this call.

- ...:

  Optional parameters.

## Value

A data.table containing the summary statistics.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4"),
  SEX     = c("M", "F", "M", NA_character_)
)
dat[, INDEX_ := .I]
#>    USUBJID    SEX INDEX_
#>     <char> <char>  <int>
#> 1:      S1      M      1
#> 2:      S2      F      2
#> 3:      S3      M      3
#> 4:      S4   <NA>      4
data.table::setkey(dat, INDEX_)
demographics_counts(dat, cell_index = dat[["INDEX_"]],
                    subjectid_var = "USUBJID",
                    stratify_by = c("TOTAL_", "SEX"),
                    strata_var = "SEX")
#>            label  description qualifiers value
#>           <char>       <char>     <char> <num>
#> 1: n_non_missing Demographics       <NA>     3
```
