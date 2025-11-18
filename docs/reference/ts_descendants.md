# Extract all descendants of a given tree-sequence node

Extract all descendants of a given tree-sequence node

## Usage

``` r
ts_descendants(ts, x, verbose = FALSE, complete = TRUE)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- x:

  An integer node ID of the ancestral node

- verbose:

  Report on the progress of ancestry path generation?

- complete:

  Does every individual in the tree sequence need to have complete
  metadata recorded? If `TRUE`, only individuals/nodes with complete
  metadata will be included in the reconstruction of ancestral
  relationships. For instance, nodes added during the coalescent
  recapitation phase will not be included because they don't have
  spatial information associated with them.

## Value

A table of descendant nodes of a given tree-sequence node all the way
down to the leaves of the tree sequence

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# find the complete descendancy information for a given individual
ts_descendants(ts, x = 62, verbose = TRUE)
#> 
#> Generating data about spatial relationships of nodes...
#> # A tibble: 4 × 12
#>   name  pop   node_id level child_id parent_id child_time parent_time child_pop
#>   <chr> <fct>   <dbl> <fct>    <int>     <int>      <dbl>       <dbl> <fct>    
#> 1 EUR_4 EUR        62 1           22        62          0       51680 EUR      
#> 2 NA    EUR        62 1           33        62        500       51680 EUR      
#> 3 EUR_1 EUR        62 2           16        33          0         500 EUR      
#> 4 EUR_2 EUR        62 2           18        33          0         500 EUR      
#> # ℹ 3 more variables: parent_pop <fct>, left_pos <dbl>, right_pos <dbl>
```
