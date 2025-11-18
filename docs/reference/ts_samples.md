# Extract names and times of individuals of interest in the current tree sequence (either all sampled individuals or those that the user simplified to)

Extract names and times of individuals of interest in the current tree
sequence (either all sampled individuals or those that the user
simplified to)

## Usage

``` r
ts_samples(ts)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

## Value

Table of individuals scheduled for sampling across space and time

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# extract the table of individuals scheduled for simulation and sampling
ts_samples(ts)
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
```
