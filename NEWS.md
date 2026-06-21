# chefStats 0.2.0

## Breaking changes

- `RR()`, `OR()`, and `RD()` now return effects in the **conventional
  direction**: risk/odds of the non-reference (intervention) arm over the
  reference arm, and `risk(non-reference) - risk(reference)` for risk
  difference. Previous versions returned the reciprocal for `RR`/`OR` and a
  sign-flipped value for `RD`. Numerical outputs from prior versions are not
  directly comparable. Confidence-interval endpoints (`RRLL`/`RRUL`,
  `ORLL`/`ORUL`, `RDLL`/`RDUL`) follow the same convention.
- `make_two_by_two_()` now returns the non-reference treatment in row 1 and
  the reference treatment in row 2, matching the convention assumed by the
  effect estimators. Code reading the matrix by `rownames` is unaffected;
  code indexing by position needs to be reviewed.

## Bug fixes

- `make_two_by_two_()` now deduplicates by `(subject, treatment)` instead of
  by subject alone. This fixes incorrect 2×2 cells for designs where the
  same `USUBJID` appears under more than one treatment value (e.g.
  crossover); parallel-group analyses are unaffected.

# chefStats 0.1.1
- Centralized the Git-Actions to {ramnog}

# chefStats 0.1.0
- Initial release of chefstats
- Contains an initial selection of statistical functions for use in the {ramnog} ecosystem.
