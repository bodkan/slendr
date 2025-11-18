# Save genotypes from the tree sequence as a VCF file

This function writes a VCF file with diploid genotypes from a given tree
sequence.

## Usage

``` r
ts_vcf(
  ts,
  path,
  chrom = "chr1",
  individuals = NULL,
  position_transform = "lambda x: np.fmax(1, x)"
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- path:

  Path to a VCF file

- chrom:

  Chromosome name to be written in the CHROM column of the VCF (default
  value will be "chr1").

- individuals:

  A vector of individuals in the tree sequence to extract genotypes
  from. If missing, all individuals present in the tree sequence will be
  saved. For a slendr-based tree sequence a character vector of
  individual names is expected. For non-slendr tree sequences, a numeric
  vector of IDs of individuals is expected.

- position_transform:

  How to transform coordinates in a tree sequence to coordinates in a
  VCF file? By default, any site with coordinate 0 is converted to a
  position 1 to ensure that the resulting VCF file adheres to the VCF
  specification. Setting this to `NULL` will disable this

## Value

No return value, called for side effects

## Details

Users should note that, as with many other tskit-based slendr functions,
`ts_vcf` is intended to provide some convenient defaults. For instance,
even for non-slendr tree sequences, it will name each individual in the
genotype columns after their integer IDs. In other words, if the
`individuals` function argument is given as `c(1, 42, 123)`, the
individuals will be named as "ind_1", "ind_42", and "ind_123", instead
of "tsk_0", "tsk_1", and "tsk_2". That said, the reticulate-based Python
interface of slendr allows calling the `write_vcf` function of tskit
directly!

By default, simulating a tree sequence with msprime and exporting the
genotypes into VCF can cause issues with some downstream software
because the VCF specification does not allow sites with the position 0.
By default `ts_vcf` automatically transforms a site with a zero
coordinate to a coordinate 1. Setting `position_transform` to NULL will
disable this, and `tsv_vcf` will save coordinates in their original
form. See this discussion for more detail:
<https://github.com/tskit-dev/tskit/issues/2838#issuecomment-1931796988>,
as well as relevant topics in the tskit documentation on this issue,
like here:
<https://tskit.dev/tskit/docs/latest/export.html#modifying-coordinates>.
