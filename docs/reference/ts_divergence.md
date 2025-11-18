# Calculate pairwise divergence between sets of individuals

Calculate pairwise divergence between sets of individuals

## Usage

``` r
ts_divergence(
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

For each pairwise calculation, either a single divergence value or a
vector of divergence values (one for each window)

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

# compute the divergence between individuals from each sample set (list of
# individual names generated in the previous step)
ts_divergence(ts, sample_sets) %>% .[order(.$divergence), ]
#> # A tibble: 6 × 3
#>   x     y     divergence
#>   <chr> <chr>      <dbl>
#> 1 AFR   EUR    0.0000736
#> 2 EUR   NEA    0.000357 
#> 3 AFR   NEA    0.000384 
#> 4 CH    NEA    0.00388  
#> 5 CH    EUR    0.00390  
#> 6 AFR   CH     0.00390  
```
