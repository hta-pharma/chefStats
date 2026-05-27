# chefStats

## Introcution to chefStats

When making an AMNOG analysis using chef, you need to pass one or more
summarization/inference functions to each endpoint.

The chefStats package makes that easier by

- Providing a collection of fast, validated functions that can be
  dropped into chef-pipelines and “just work”
- Guiding the creation of new (validated) functions if you need one that
  doesn’t yet exist
  ([`use_chefStats()`](https://hta-pharma.github.io/chefStats/reference/use_chefStats.md))
