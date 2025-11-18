# Extract list with tree sequence metadata saved by SLiM

Extract list with tree sequence metadata saved by SLiM

## Usage

``` r
ts_metadata(ts)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

## Value

List of metadata fields extracted from the tree-sequence object

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# extract the list of metadata information from the tree sequence
ts_metadata(ts)
#> $version
#> [1] "slendr_1.3.0"
#> 
#> $description
#> [1] ""
#> 
#> $sampling
#> # A tibble: 13 × 3
#>    name   time pop  
#>    <chr> <int> <chr>
#>  1 NEA_1 70000 NEA  
#>  2 NEA_2 40000 NEA  
#>  3 AFR_1     0 AFR  
#>  4 AFR_2     0 AFR  
#>  5 AFR_3     0 AFR  
#>  6 AFR_4     0 AFR  
#>  7 AFR_5     0 AFR  
#>  8 CH_1      0 CH   
#>  9 EUR_1     0 EUR  
#> 10 EUR_2     0 EUR  
#> 11 EUR_3     0 EUR  
#> 12 EUR_4     0 EUR  
#> 13 EUR_5     0 EUR  
#> 
#> $sample_names
#>  [1] "NEA_1" "NEA_2" "AFR_1" "AFR_2" "AFR_3" "AFR_4" "AFR_5" "CH_1"  "EUR_1"
#> [10] "EUR_2" "EUR_3" "EUR_4" "EUR_5"
#> 
#> $subset_names
#>  [1] "NEA_1" "NEA_2" "AFR_1" "AFR_2" "AFR_3" "AFR_4" "AFR_5" "CH_1"  "EUR_1"
#> [10] "EUR_2" "EUR_3" "EUR_4" "EUR_5"
#> 
#> $sample_ids
#> NULL
#> 
#> $map
#> NULL
#> 
#> $arguments
#> $arguments$BURNIN_LENGTH
#> [1] 0
#> 
#> $arguments$MAX_ATTEMPTS
#> [1] 1
#> 
#> $arguments$RECOMBINATION_RATE
#> [1] 1e-08
#> 
#> $arguments$SEED
#> [1] 314159
#> 
#> $arguments$SEQUENCE_LENGTH
#> [1] 500000
#> 
#> $arguments$SIMULATION_LENGTH
#> [1] 216667
#> 
#> 
```
