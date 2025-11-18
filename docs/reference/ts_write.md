# Save a tree sequence to a file

Save a tree sequence to a file

## Usage

``` r
ts_write(ts, file)
```

## Arguments

- ts:

  Tree sequence object loaded by `ts_read`

- file:

  File to which the tree sequence should be saved

## Value

No return value, called for side effects

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree sequence
ts <- ts_read(slendr_ts, model)

# save the tree-sequence object to a different location
another_file <- paste(tempfile(), ".trees")
ts_write(ts, another_file)
```
