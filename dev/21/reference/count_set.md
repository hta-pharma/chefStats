# Produce counts of Number of subjects, number of events, number of subjects with events, and proportion of subjects with events

A short cut - instead of calling the individual function (`n_sub`,
`n_event`, `n_subj_event`, `p_subj_event`), one call to this function
will produce all the described functions. This can be useful to save
compute time as inside the chef pipeline there will be fewer iterations.

## Usage

``` r
count_set(dat, event_index, cell_index, subjectid_var, ...)
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

a data.table containing all statistical outputs

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
count_set(dat, event_index = c(1L, 2L), cell_index = dat[["INDEX_"]],
          subjectid_var = "USUBJID")
#>                           description qualifiers  label value
#>                                <char>     <char> <char> <num>
#> 1:                 Number of subjects       <NA>      N     4
#> 2:                   Number of events       <NA>      E     2
#> 3:     Number of subjects with events       <NA>      n     2
#> 4: Proportion of subjects with events       <NA>    (%)    50
```
