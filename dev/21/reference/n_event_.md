# Building-block: Number of events (multiple events counted multiple times)

Building-block: Number of events (multiple events counted multiple
times)

## Usage

``` r
n_event_(dat, intersect_index)
```

## Arguments

- dat:

  data.table. The analysis data set.

- intersect_index:

  A vector of intergers referencing the rows of `dat` that match
  both (1) the population to be analyzed and (2) the defenition of an
  event

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
n_event_(dat, intersect_index = c(1L, 2L))
#> [1] 2
```
