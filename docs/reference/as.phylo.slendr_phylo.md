# Convert an annotated `slendr_phylo` object to a `phylo` object

This function servers as a workaround around a ggtree error:
`Error in UseMethod("as.phylo") : no applicable method for 'as.phylo' applied to an object of class "c('phylo', 'slendr_phylo')"`

## Usage

``` r
# S3 method for class 'slendr_phylo'
as.phylo(x, ...)
```

## Arguments

- x:

  Tree object of the class `slendr_phylo`

- ...:

  Additional (unused) arguments of the `as.phylo` S3 method

## Value

Standard phylogenetic tree object implemented by the R package ape
