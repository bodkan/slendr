# Extract (spatio-)temporal ancestral history for given nodes/individuals

Extract (spatio-)temporal ancestral history for given nodes/individuals

## Usage

``` r
ts_ancestors(ts, x, verbose = FALSE, complete = TRUE)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- x:

  Either an individual name or an integer node ID

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

A table of ancestral nodes of a given tree-sequence node all the way up
to the root of the tree sequence

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# find the complete ancestry information for a given individual
ts_ancestors(ts, "EUR_1", verbose = TRUE)
#> Collecting ancestors of EUR_1 [1/1]...
#> 
#> Generating data about spatial relationships of nodes...
#> # A tibble: 207 × 12
#>    name  pop   node_id level child_id parent_id child_time parent_time child_pop
#>    <chr> <fct>   <int> <fct>    <int>     <int>      <dbl>       <dbl> <fct>    
#>  1 EUR_1 EUR        16 1           16        33          0         500 EUR      
#>  2 EUR_1 EUR        16 2           33        43        500       21830 EUR      
#>  3 EUR_1 EUR        16 2           33        47        500       28730 EUR      
#>  4 EUR_1 EUR        16 2           33        49        500       30380 EUR      
#>  5 EUR_1 EUR        16 2           33        51        500       35360 EUR      
#>  6 EUR_1 EUR        16 2           33        54        500       39380 EUR      
#>  7 EUR_1 EUR        16 2           33        57        500       41510 EUR      
#>  8 EUR_1 EUR        16 2           33        58        500       44810 EUR      
#>  9 EUR_1 EUR        16 2           33        59        500       46040 EUR      
#> 10 EUR_1 EUR        16 2           33        62        500       51680 EUR      
#> # ℹ 197 more rows
#> # ℹ 3 more variables: parent_pop <fct>, left_pos <dbl>, right_pos <dbl>
```
