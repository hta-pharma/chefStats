# Wrapper to prepare data for Hedges G

Wrapper to prepare data for Hedges G

## Usage

``` r
hedges_g(dat, reference_val, safe_mode = FALSE, ...)
```

## Arguments

- dat:

  data.table. The analysis data set.

- reference_val:

  character. The reference value of the treatment variable in the data.

- safe_mode:

  Boolean determining if the function should fail when given input that
  cannot be calculated (`safe_mode = TRUE`), or if it should silently
  return a `NA` value (default

- ...:

  optional arguments to

## Value

A list containing Hedges G statistics.

## Examples

``` r
dat <- data.table::data.table(
  treatment_val = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo"),
  stat_var      = rep(c("N_sub", "mean_value", "sd_value"), 2),
  stat_val      = c(50, 12.3, 2.1, 50, 10.1, 2.5)
)
hedges_g(dat, reference_val = "Placebo")
#> $pe
#> [1] 0.9456143
#> 
#> $upper
#> [1] 1.358934
#> 
#> $lower
#> [1] 0.5322945
#> 
```
