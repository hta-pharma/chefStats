# Calculate number of events by treatment and stratum

Calculate the number of events by treatment and stratum.

## Usage

``` r
n_event(dat, event_index, cell_index, subjectid_var, ...)
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

- ...:

  Optional parameters.

## Value

A data.table containing the number of events for the given combination
of treatment and stratum.

## Examples

``` r
dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3", "S4"))
dat[, INDEX_ := .I]
#>    USUBJID INDEX_
#>     <char>  <int>
#> 1:      S1      1
#> 2:      S2      2
#> 3:      S3      3
#> 4:      S4      4
data.table::setkey(dat, INDEX_)
n_event(dat, event_index = c(1L, 2L), cell_index = dat[["INDEX_"]],
        subjectid_var = "USUBJID")
#>         description qualifiers  label value
#>              <char>     <char> <char> <num>
#> 1: Number of events       <NA>      E     2
```
