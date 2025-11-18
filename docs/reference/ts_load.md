# Read a tree sequence from a file

Deprecated function. Please use `ts_read` instead.

## Usage

``` r
ts_load(file, model = NULL)
```

## Arguments

- file:

  A path to the tree-sequence file (either originating from a slendr
  model or a standard non-slendr tree sequence).

- model:

  Optional `slendr_model` object which produced the tree-sequence
  `file`. Used for adding various annotation data and metadata to the
  standard tskit tree-sequence object.
