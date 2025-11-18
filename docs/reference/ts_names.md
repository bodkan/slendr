# Extract names of individuals in a tree sequence

Extract names of individuals in a tree sequence

## Usage

``` r
ts_names(ts, split = NULL)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- split:

  Should sample names in the tree sequence be split by a column (a
  population or time column)? Default is `NULL` and all names of samples
  will be returned as a single character vector. If set to "pop" or
  "time", a list of character vectors will be returned, one vector for
  each unique "pop" or "time" grouping.

## Value

A vector of character sample names. If `split` is specified, a list of
such vectors is returned, one element of the list per population or
sampling time.
