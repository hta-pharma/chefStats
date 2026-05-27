# Breslow-Day test

Breslow-Day test

## Usage

``` r
breslowdaytest_(x, odds_ratio = NA, correct = FALSE)
```

## Arguments

- x:

  a 2x2xK contingency table

- odds_ratio:

  Odds Ration (default = NA)

- correct:

  if TRUE Tarones correction is returned. Default = FALSE.

## Value

A vector with three values statistic - Breslow and Day test statistic
pval - p value evtl. based on the Tarone test statistic using a
\\\chi^2(K-1)\\ distribution
