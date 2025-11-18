# Define sampling events for a given set of populations

Schedule sampling events at specified times and, optionally, a given set
of locations on a landscape

## Usage

``` r
schedule_sampling(model, times, ..., locations = NULL, strict = FALSE)
```

## Arguments

- model:

  Object of the class `slendr_model`

- times:

  Integer vector of times (in model time units) at which to schedule
  remembering of individuals in the tree-sequence

- ...:

  Lists of two elements (`slendr_pop` population object-\<number of
  individuals to sample), representing from which populations should how
  many individuals be remembered at times given by `times`

- locations:

  List of vector pairs, defining two-dimensional coordinates of
  locations at which the closest number of individuals from given
  populations should be sampled. If `NULL` (the default), individuals
  will be sampled randomly throughout their spatial boundary.

- strict:

  Should any occurence of a population not being present at a given time
  result in an error? Default is `FALSE`, meaning that invalid sampling
  times for any populations will be quietly ignored.

## Value

Data frame with three columns: time of sampling, population to sample
from, how many individuals to sample

## Details

If both times and locations are given, the the sampling will be
scheduled on each specified location in each given time-point. Note that
for the time-being, in the interest of simplicity, no sanity checks are
performed on the locations given except the restriction that the
sampling points must fall within the bounding box around the simulated
world map. Other than that, slendr will simply instruct its SLiM backend
script to sample individuals as close to the sampling points given as
possible, regardless of whether those points lie within a population
spatial boundary at that particular moment of time.

Optionally, a name of a single sample from a population can be given,
which will then replace the generic format of
`"\{population\}_\{number\}"` name. See example for more detail.

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
path <- system.file("extdata/models/introgression", package = "slendr")
model <- read_model(path)

# afr, eur, and nea objects would normally be created before slendr model
# compilation, but here we take them out of the model object already compiled for
# this example (in a standard slendr simulation pipeline, this wouldn't be necessary)
afr <- model$populations[["AFR"]]
eur <- model$populations[["EUR"]]
nea <- model$populations[["NEA"]]

# schedule the recording of 10 African and 100 European individuals from a
# given model at 20 ky, 10 ky, 5ky ago and at present-day (time 0)
schedule_amh <- schedule_sampling(
  model, times = c(20000, 10000, 5000, 0),
  list(afr, 10), list(eur, 100)
)
# schedule the recording of the Vindija Neanderthal genome
schedule_nea <- schedule_sampling(model, times = 40000, list(nea, 1, "Vindija"))

# the result of `schedule_sampling` is a simple data frame (note that the locations
# of sampling locations have `NA` values because the model is non-spatial)
schedule <- rbind(schedule_amh, schedule_nea)
schedule
#> # A tibble: 9 × 8
#>    time pop       n name    y_orig x_orig y     x    
#>   <int> <chr> <int> <chr>   <lgl>  <lgl>  <lgl> <lgl>
#> 1     0 AFR      10 NA      NA     NA     NA    NA   
#> 2     0 EUR     100 NA      NA     NA     NA    NA   
#> 3  5000 AFR      10 NA      NA     NA     NA    NA   
#> 4  5000 EUR     100 NA      NA     NA     NA    NA   
#> 5 10000 AFR      10 NA      NA     NA     NA    NA   
#> 6 10000 EUR     100 NA      NA     NA     NA    NA   
#> 7 20000 AFR      10 NA      NA     NA     NA    NA   
#> 8 20000 EUR     100 NA      NA     NA     NA    NA   
#> 9 40000 NEA       1 Vindija NA     NA     NA    NA   

# simulate a tree sequence
ts <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = schedule)

# inspect the recorded table of samples
ts_samples(ts)
#> # A tibble: 441 × 3
#>    name     time pop  
#>    <chr>   <dbl> <chr>
#>  1 Vindija 40000 NEA  
#>  2 AFR_1   20000 AFR  
#>  3 AFR_2   20000 AFR  
#>  4 AFR_3   20000 AFR  
#>  5 AFR_4   20000 AFR  
#>  6 AFR_5   20000 AFR  
#>  7 AFR_6   20000 AFR  
#>  8 AFR_7   20000 AFR  
#>  9 AFR_8   20000 AFR  
#> 10 AFR_9   20000 AFR  
#> # ℹ 431 more rows
```
