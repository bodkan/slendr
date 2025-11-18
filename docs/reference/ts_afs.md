# Compute the allele frequency spectrum (AFS)

This function computes the AFS with respect to the given set of
individuals or nodes.

## Usage

``` r
ts_afs(
  ts,
  sample_sets = NULL,
  mode = c("site", "branch", "node"),
  windows = NULL,
  span_normalise = FALSE,
  polarised = TRUE
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- sample_sets:

  A list (optionally a named list) of character vectors with individual
  names (one vector per set). If NULL, allele frequency spectrum for all
  individuals in the tree sequence will be computed.

- mode:

  The mode for the calculation ("sites" or "branch")

- windows:

  Coordinates of breakpoints between windows. The first coordinate (0)
  and the last coordinate (equal to `ts$sequence_length`) are added
  automatically)

- span_normalise:

  Argument passed to tskit's `allele_frequency_spectrum` method

- polarised:

  When TRUE (the default) the allele frequency spectrum will not be
  folded (i.e. the counts will assume knowledge of which allele is
  ancestral, and which is derived, which is known in a simulation)

## Value

Allele frequency spectrum values for the given sample set. Note that the
contents of the first and last elements of the AFS might surprise you.
Read the links in the description for more detail on how tskit handles
things.

## Details

For more information on the format of the result and dimensions, in
particular the interpretation of the first and the last element of the
AFS, please see the tskit manual at
<https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.allele_frequency_spectrum>
and the example section dedicated to AFS at
<https://tskit.dev/tutorials/analysing_tree_sequences.html#zeroth-and-final-entries-in-the-afs>.

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model) %>% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

samples <- ts_samples(ts) %>% .[.$pop %in% c("AFR", "EUR"), ]

# compute AFS for the given set of individuals
ts_afs(ts, sample_sets = list(samples$name))
#>  [1] 1018   73   21    7    0    6    2    0    0    0   13    0    0    0    0
#> [16]    4    0    4    6   16  955
```
