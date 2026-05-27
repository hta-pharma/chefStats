# Package index

## By strata and treatment level

Function that operate by strata level and treatment level

- [`n_subj()`](https://hta-pharma.github.io/chefStats/reference/n_subj.md)
  : Calculate number of subjects by treatment and stratum
- [`n_event()`](https://hta-pharma.github.io/chefStats/reference/n_event.md)
  : Calculate number of events by treatment and stratum
- [`n_subj_event()`](https://hta-pharma.github.io/chefStats/reference/n_subj_event.md)
  : Calculate number of subjects with events by treatment and stratum
- [`p_subj_event()`](https://hta-pharma.github.io/chefStats/reference/p_subj_event.md)
  : Calculate percentage of subjects with events by treatment and
  stratum
- [`count_set()`](https://hta-pharma.github.io/chefStats/reference/count_set.md)
  : Produce counts of Number of subjects, number of events, number of
  subjects with events, and proportion of subjects with events

## By strata, across treatment levels

Function that operate by strata level but across treatment levels

- [`RR()`](https://hta-pharma.github.io/chefStats/reference/RR.md) :
  Relative Risk calculation
- [`OR()`](https://hta-pharma.github.io/chefStats/reference/OR.md) :
  Wrapper for Odds Ratio calculation
- [`RD()`](https://hta-pharma.github.io/chefStats/reference/RD.md) :
  Wrapper for Risk Difference calculation
- [`p_val()`](https://hta-pharma.github.io/chefStats/reference/p_val.md)
  : Wrapper for P-value calculations

## Across strata, across treatment level

Function that operate across strata levels and across treatment levels

- [`p_val_interaction()`](https://hta-pharma.github.io/chefStats/reference/p_val_interaction.md)
  : Wrapper for P-value of interaction tests

## Building-block functions

These are used as building blocks when making the chef-facing functions.
They are exported so that they can also be use in chefCriteria but they
cannot be called directly in a chef pipeline

- [`breslowdaytest_()`](https://hta-pharma.github.io/chefStats/reference/breslowdaytest_.md)
  : Breslow-Day test
- [`barnard_test_()`](https://hta-pharma.github.io/chefStats/reference/barnard_test_.md)
  : Barnards Unconditional Exact Test with trial data
- [`make_two_by_two_by_k_()`](https://hta-pharma.github.io/chefStats/reference/make_two_by_two_by_k_.md)
  : Make 2x2xk contingency tables from summarized adam data. This
  function is NOT generalized
- [`make_two_by_two_()`](https://hta-pharma.github.io/chefStats/reference/make_two_by_two_.md)
  : Make a two-by-two table
- [`n_subj_()`](https://hta-pharma.github.io/chefStats/reference/n_subj_.md)
  : Building-block: Number of subjects
- [`n_event_()`](https://hta-pharma.github.io/chefStats/reference/n_event_.md)
  : Building-block: Number of events (multiple events counted multiple
  times)
- [`n_subj_event_()`](https://hta-pharma.github.io/chefStats/reference/n_subj_event_.md)
  : Building-block: Number of subjects with at least one event
- [`p_subj_event_()`](https://hta-pharma.github.io/chefStats/reference/p_subj_event_.md)
  : Building-block: Proportion of subjects having at least one event
- [`use_chefStats()`](https://hta-pharma.github.io/chefStats/reference/use_chefStats.md)
  : Make new chefStats functions

## Misc

Non-assigned

- [`demographics_continuous()`](https://hta-pharma.github.io/chefStats/reference/demographics_continuous.md)
  : Calculate summary statistics for demographics on a continuous
  variable
- [`demographics_counts()`](https://hta-pharma.github.io/chefStats/reference/demographics_counts.md)
  : Calculate basic summary statistics for demographics
- [`hedges_g()`](https://hta-pharma.github.io/chefStats/reference/hedges_g.md)
  : Wrapper to prepare data for Hedges G
- [`mean_value()`](https://hta-pharma.github.io/chefStats/reference/mean_value.md)
  : Calculate mean value
- [`n_event_100y()`](https://hta-pharma.github.io/chefStats/reference/n_event_100y.md)
  : Calculate number of events per 100 years of exposure
- [`obs_time_by_trt()`](https://hta-pharma.github.io/chefStats/reference/obs_time_by_trt.md)
  : Calculate observation time by treatment
- [`p_subj_event_by_trt()`](https://hta-pharma.github.io/chefStats/reference/p_subj_event_by_trt.md)
  : Calculate percentage of subjects with events
- [`sd_value()`](https://hta-pharma.github.io/chefStats/reference/sd_value.md)
  : Standard deviation
