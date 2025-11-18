# Calculate pairwise statistics between sets of individuals

For a discussion on the difference between "site", "branch", and "node"
options of the `mode` argument, please see the tskit documentation at
<https://tskit.dev/tskit/docs/stable/stats.html#sec-stats-mode>.

## Usage

``` r
ts_fst(
  ts,
  sample_sets,
  mode = c("site", "branch", "node"),
  windows = NULL,
  span_normalise = TRUE
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- sample_sets:

  A list (optionally a named list) of character vectors with individual
  names (one vector per set)

- mode:

  The mode for the calculation ("sites" or "branch")

- windows:

  Coordinates of breakpoints between windows. The first coordinate (0)
  and the last coordinate (equal to `ts$sequence_length`) do not have to
  be specified as they are added automatically.

- span_normalise:

  Divide the result by the span of the window? Default TRUE, see the
  tskit documentation for more detail.

## Value

For each pairwise calculation, either a single Fst value or a vector of
Fst values (one for each window)

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model) %>% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

# compute F_st between two sets of individuals in a given tree sequence ts
ts_fst(ts, sample_sets = list(afr = c("AFR_1", "AFR_2", "AFR_3"),
                              eur = c("EUR_1", "EUR_2")))
#> # A tibble: 1 × 3
#>   x     y       Fst
#>   <chr> <chr> <dbl>
#> 1 afr   eur   0.324
```
