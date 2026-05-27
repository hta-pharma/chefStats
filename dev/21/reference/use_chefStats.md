# Make new chefStats functions

Make new chefStats functions

## Usage

``` r
use_chefStats(
  fn_name,
  fn_type = c("stat_by_strata_by_trt", "stat_by_strata_across_trt",
    "stat_across_strata_across_trt"),
  file_name = NULL
)
```

## Arguments

- fn_name:

  Name of the function. If file_name is `NULL` then `fn_name` will be
  used to make the file name

- fn_type:

  Type of statistical function, one of `stat_by_strata_by_trt`,
  `stat_by_strata_across_trt`, or `stat_across_strata_across_trt`

- file_name:

  Name of the file to store function in (if different from `fn_name`)

## Value

Nothing, run for side-effect (writes file to disk)

## Examples

``` r
if (FALSE) { # \dontrun{
use_chefStats(fn_name = "my_stat_fn", fn_type = "stat_by_strata_by_trt")
} # }
```
