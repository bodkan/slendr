# Check that all trees in the tree sequence are fully coalesced

Check that all trees in the tree sequence are fully coalesced

## Usage

``` r
ts_coalesced(ts, return_failed = FALSE)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- return_failed:

  Report back which trees failed the coalescence check?

## Value

TRUE or FALSE value if `return_failed = FALSE`, otherwise a vector of
(tskit Python 0-based) indices of trees which failed the coalescence
test

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

ts_coalesced(ts) # is the tree sequence fully coalesced? (TRUE or FALSE)
#> [1] TRUE

# returns a vector of tree sequence segments which are not coalesced
not_coalesced <- ts_coalesced(ts, return_failed = TRUE)
```
