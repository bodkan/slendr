# Get a tree from a given tree sequence

For more information about optional keyword arguments see tskit
documentation:
<https://tskit.dev/tskit/docs/stable/python-api.html#the-treesequence-class>

## Usage

``` r
ts_tree(ts, i, mode = c("index", "position"), ...)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- i:

  Position of the tree in the tree sequence. If `mode = "index"`, an
  i-th tree will be returned (in zero-based indexing as in tskit), if
  `mode = "position"`, a tree covering the i-th base of the simulated
  genome will be returned (again, in tskit's indexing).

- mode:

  How should the `i` argument be interpreted? Either "index" as an i-th
  tree in the sequence of genealogies, or "position" along the simulated
  genome.

- ...:

  Additional keyword arguments accepted by
  `tskit.TreeSequence.at and tskit.TreeSequence.at_index` methods

## Value

Python-reticulate-based object of the class tskit.trees.Tree

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# extract the zero-th tree in the tree sequence
tree <- ts_tree(ts, i = 0)

# extract the tree at a position in the tree sequence
tree <- ts_tree(ts, i = 100000, mode = "position")
```
