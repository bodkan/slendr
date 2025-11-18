# Convert genotypes to the EIGENSTRAT file format

EIGENSTRAT data produced by this function can be used by the admixr R
package (<https://bodkan.net/admixr/>).

## Usage

``` r
ts_eigenstrat(ts, prefix, chrom = "chr1", outgroup = NULL)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- prefix:

  EIGENSTRAT trio prefix

- chrom:

  The name of the chromosome in the EIGENSTRAT snp file (default "chr1")

- outgroup:

  Should a formal, artificial outgroup be added? If `NULL` (default), no
  outgroup is added. A non-NULL character name will serve as the name of
  the outgroup in an ind file.

## Value

Object of the class EIGENSTRAT created by the admixr package

## Details

In case an outgroup was not formally specified in a slendr model which
generated the tree sequence data, it is possible to artificially create
an outgroup sample with the name specified by the `outgroup` argument,
which will carry all ancestral alleles (i.e. value "2" in a geno file
for each position in a snp file).
