# Building-block: Number of subjects

Building-block: Number of subjects

## Usage

``` r
n_subj_(dat, cell_index, subjectid_var)
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

## Value

An integer

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3"),
  VALUE   = c(1.1, 2.2, 3.3)
)
dat[, INDEX_ := .I]
#>    USUBJID VALUE INDEX_
#>     <char> <num>  <int>
#> 1:      S1   1.1      1
#> 2:      S2   2.2      2
#> 3:      S3   3.3      3
data.table::setkey(dat, INDEX_)
n_subj_(dat, cell_index = dat[["INDEX_"]], subjectid_var = "USUBJID")
#> [1] 3
```
