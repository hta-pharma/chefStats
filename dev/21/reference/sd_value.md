# Standard deviation

Standard deviation

## Usage

``` r
sd_value(dat, event_index, cell_index, subjectid_var, var, ...)
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

- var:

  Character. Name of the variable in the analysis data that is subject
  to the statistics.

- ...:

  Optional parameters.

## Value

A data.table containing the standard deviation.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3"),
  AVAL    = c(1.5, 2.5, 3.5)
)
dat[, INDEX_ := .I]
#>    USUBJID  AVAL INDEX_
#>     <char> <num>  <int>
#> 1:      S1   1.5      1
#> 2:      S2   2.5      2
#> 3:      S3   3.5      3
data.table::setkey(dat, INDEX_)
sd_value(dat, event_index = dat[["INDEX_"]], cell_index = dat[["INDEX_"]],
         subjectid_var = "USUBJID", var = "AVAL")
#>     label        description qualifiers value
#>    <char>             <char>     <char> <num>
#> 1:     SD Standard deviation       AVAL     1
```
