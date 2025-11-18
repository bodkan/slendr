# Calculate Tajima's D for given sets of individuals

For a discussion on the difference between "site" and "branch" options
of the `mode` argument, please see the tskit documentation at
<https://tskit.dev/tskit/docs/stable/stats.html#sec-stats-mode>

## Usage

``` r
ts_tajima(ts, sample_sets, mode = c("site", "branch", "node"), windows = NULL)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- sample_sets:

  A list (optionally a named list) of character vectors with individual
  names (one vector per set). If a simple vector is provided, it will be
  interpreted as `as.list(sample_sets)`, meaning that a given statistic
  will be calculated for each individual separately.

- mode:

  The mode for the calculation ("sites" or "branch")

- windows:

  Coordinates of breakpoints between windows. The first coordinate (0)
  and the last coordinate (equal to `ts$sequence_length`) are added
  automatically)

## Value

For each set of individuals either a single Tajima's D value or a vector
of Tajima's D values (one for each window)

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model) %>% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

# calculate Tajima's D for given sets of individuals in a tree sequence ts
ts_tajima(ts, list(eur = c("EUR_1", "EUR_2", "EUR_3", "EUR_4", "EUR_5"),
                   nea = c("NEA_1", "NEA_2")))
#> # A tibble: 2 × 2
#>   set       D
#>   <chr> <dbl>
#> 1 eur   -1.00
#> 2 nea    2.16
```
