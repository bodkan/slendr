# Extract ancestry tracts from a tree sequence (EXPERIMENTAL)

Extract a data frame with coordinates of ancestry tracts from a given
tree sequence.

## Usage

``` r
ts_tracts(
  ts,
  census,
  squashed = TRUE,
  source = NULL,
  target = NULL,
  quiet = FALSE
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- census:

  Census time. See the documentation linked in the Details for more
  information. If a slendr-specific tree sequence was provided as `ts`,
  the census time is expected to be given in slendr model-specific time
  units, and must correspond to some gene-flow event encoded by the
  model.

- squashed:

  Should ancestry tracts be squashed (i.e., should continuous tracts
  that can be traced to different ancestral nodes be merged)? Default is
  `TRUE`. If `FALSE`, these effectively continuous ancestry tracts will
  be split into individual segments, each assigned to a specific
  ancestral node ID (recorded in a column `ancestor_id`).

- source:

  From which source population to extract tracts for? if `NULL` (the
  default), ancestry tracts for all populations contributing gene flow
  at the census time will be reported. Otherwise, ancestry tracts from
  only specified source populations will be extracted. Note that this
  option is ignored for non-slendr tree sequences!

- target:

  Similar purpose as `source` above, except that it filters for tracts
  discovered in the target population(s)

- quiet:

  Should the default summary output of the `tspop` Python package be
  silenced? Default is `FALSE`.

## Value

A data frame containing coordinates of ancestry tracts

## Details

This function implements an R-friendly interface to an algorithm for
extracting ancestry tracts provided by the Python module tspop
<https://tspop.readthedocs.io/en/latest/> and developed by Georgia
Tsambos. Please make sure to cite the paper which describes the
algorithm in detail:
[doi:10.1093/bioadv/vbad163](https://doi.org/10.1093/bioadv/vbad163) .
For more technical details, see also the tutorial at:
<https://tspop.readthedocs.io/en/latest/basicusage.html>.

In general, when using this function on a slendr-generated tree
sequence, please be aware that the output changes slightly to what you
would get by running the pure `tspop.get_pop_ancestry()` in Python.
First, `ts_tracts()` populates the output data frame with additional
metadata (such as names of individuals or populations). Additionally,
for slendr models, it is specifically designed to only return ancestry
tracts originating to a an ancestral population which contributed its
ancestry during a gene-flow event which started at a specific time
(i.e., scheduled in a model via the
[`gene_flow()`](https://slendr.net/reference/gene_flow.md)) function. It
does not return every single ancestry tracts present in the tree
sequence for every single sample node (and every single potential
ancestry population) as does the `tspop.get_pop_ancestry()` Python
method.

That said, when run on a tree sequence which does not originate from a
slendr simulation, the behavior of `ts_tracts()` is identical to that of
the underlying `tspop.get_pop_ancestry()`.

As of the current version of slendr, `ts_tracts()` only works for
slendr/msprime sequences but not on slendr/SLiM tree sequences. Support
for slendr-generated SLiM tree sequences is in development. Tracts from
tree sequences originating from non-slendr msprime and SLiM simulations
are not restricted in any way and, as mentioned in the previous
paragraph, `ts_tracts()` in this situation effectively reduces to the
standard `tspop.get_pop_ancestry()` call.

## Examples

``` r
init_env(quiet = TRUE)

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_msprime.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(file = slendr_ts, model = model)

# extract Neanderthal ancestry tracts (i.e. those corresponding to the
# census event at the gene-flow time at 55000 kya as scheduled by
# the simulation which produced the tree sequence)
nea_tracts <- ts_tracts(ts, census = 55000, source = "NEA")
#> 
#> PopAncestry summary
#> Number of ancestral populations:     4
#> Number of sample chromosomes:        26
#> Number of ancestors:             187
#> Total length of genomes:         26000000.000000
#> Ancestral coverage:          24000000.000000
#> 
nea_tracts
#> # A tibble: 0 × 10
#> # ℹ 10 variables: name <chr>, haplotype <int>, time <dbl>, pop <chr>,
#> #   source_pop <fct>, left <dbl>, right <dbl>, length <dbl>,
#> #   source_pop_id <dbl>, node_id <dbl>
```
