# Calculate summary statistics for demographics on a continuous variable

Calculate a set of summary statistics (mean, median, sd, min, max,
n_non_missing, n_missing) on a continuous variable for demographics
endpoints.

## Usage

``` r
demographics_continuous(dat, event_index, cell_index, subjectid_var, var, ...)
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

  character. Name of the variable in the analysis data that is subject
  to the statistics.

- ...:

  Optional parameters.

## Value

A data.table containing the summary statistics for the given continuous
variable in the analysis data set.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4"),
  AGE     = c(45, 52, 60, 38)
)
dat[, INDEX_ := .I]
#>    USUBJID   AGE INDEX_
#>     <char> <num>  <int>
#> 1:      S1    45      1
#> 2:      S2    52      2
#> 3:      S3    60      3
#> 4:      S4    38      4
data.table::setkey(dat, INDEX_)
demographics_continuous(dat, event_index = dat[["INDEX_"]],
                        cell_index = dat[["INDEX_"]],
                        subjectid_var = "USUBJID", var = "AGE")
#>            label  description qualifiers     value
#>           <char>       <char>     <char>     <num>
#> 1:          mean Demographics        AGE 48.750000
#> 2:        median Demographics        AGE 48.500000
#> 3:            sd Demographics        AGE  9.429563
#> 4:           min Demographics        AGE 38.000000
#> 5:           max Demographics        AGE 60.000000
#> 6: n_non_missing Demographics        AGE  4.000000
#> 7:     n_missing Demographics        AGE  0.000000
```
