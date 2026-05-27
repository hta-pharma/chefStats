# Building-block: Proportion of subjects having at least one event

Building-block: Proportion of subjects having at least one event

## Usage

``` r
p_subj_event_(dat, cell_index, intersect_index, subjectid_var)
```

## Arguments

- dat:

  data.table. The analysis data set.

- cell_index:

  A vector of integers referencing the rows of `dat` (as specified by
  the `INDEX_` column in `dat`) that match the population to be
  analyzed. See the "Endpoint Events" vignette in ramnog for more
  information.

- intersect_index:

  A vector of intergers referencing the rows of `dat` that match
  both (1) the population to be analyzed and (2) the defenition of an
  event

- subjectid_var:

  character. Name of the subject identifier variable in the data
  (default is "USUBJID").

## Value

an integer value

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
p_subj_event_(dat, cell_index = 1:3, intersect_index = c(1L, 2L),
              subjectid_var = "USUBJID")
#> [1] 66.66667
```
