# Calculate the density of segregating sites for the given sets of individuals

Calculate the density of segregating sites for the given sets of
individuals

## Usage

``` r
ts_segregating(
  ts,
  sample_sets,
  mode = c("site", "branch", "node"),
  windows = NULL,
  span_normalise = FALSE
)
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

- span_normalise:

  Divide the result by the span of the window? Default TRUE, see the
  tskit documentation for more detail.

## Value

For each set of individuals either a single diversity value or a vector
of diversity values (one for each window)

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model) %>% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

# collect sampled individuals from all populations in a list
sample_sets <- ts_samples(ts) %>%
  split(., .$pop) %>%
  lapply(function(pop) pop$name)

ts_segregating(ts, sample_sets)
#> # A tibble: 4 × 2
#>   set   segsites
#>   <chr>    <dbl>
#> 1 AFR          1
#> 2 CH           0
#> 3 EUR        138
#> 4 NEA          6
```
