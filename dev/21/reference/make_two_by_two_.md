# Make a two-by-two table

Makes a two-by-two contingency table which takes the visual form:

|            |         |          |
|------------|---------|----------|
|            | Outcome | observed |
|            | Yes     | No       |
| Treatment  | A       | B        |
| Comparator | C       | D        |

Where A, B, C, and D are the distinct number of subjects satisfying the
criteria of each 2x2 cell.

Only observations that have a Treatment value recorded are returned.

## Usage

``` r
make_two_by_two_(
  dat,
  event_index,
  cell_index,
  treatment_var,
  treatment_refval,
  subjectid_var
)
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

- treatment_var:

  character. The name of the treatment variable in `dat`.

- treatment_refval:

  character. The reference value of the treatment variable in `dat`.

- subjectid_var:

  character. Name of the subject identifier variable in `dat` (default
  is "USUBJID").

## Value

A matrix

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
  TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
)
dat[, INDEX_ := .I]
#>    USUBJID     TRT INDEX_
#>     <char>  <char>  <int>
#> 1:      S1  Active      1
#> 2:      S2  Active      2
#> 3:      S3  Active      3
#> 4:      S4 Placebo      4
#> 5:      S5 Placebo      5
#> 6:      S6 Placebo      6
data.table::setkey(dat, INDEX_)
make_two_by_two_(dat, event_index = c(1L, 2L, 4L),
                 cell_index = dat[["INDEX_"]],
                 treatment_var = "TRT", treatment_refval = "Placebo",
                 subjectid_var = "USUBJID")
#>         outcome_YES outcome_NO
#> Active            2          1
#> Placebo           1          2
```
