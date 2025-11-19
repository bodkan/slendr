# Run a slendr model in msprime

This function will execute a built-in msprime script and run a compiled
slendr demographic model.

## Usage

``` r
msprime(
  model,
  sequence_length,
  recombination_rate,
  samples = NULL,
  random_seed = NULL,
  verbose = FALSE,
  debug = FALSE,
  run = TRUE,
  path = NULL,
  coalescent_only = TRUE
)
```

## Arguments

- model:

  Model object created by the `compile` function

- sequence_length:

  Total length of the simulated sequence (in base-pairs)

- recombination_rate:

  Recombination rate of the simulated sequence (in recombinations per
  basepair per generation)

- samples:

  A data frame of times at which a given number of individuals should be
  remembered in the tree-sequence (see `schedule_sampling` for a
  function that can generate the sampling schedule in the correct
  format). If missing, only individuals present at the end of the
  simulation will be recorded in the final tree-sequence file.

- random_seed:

  Random seed (if `NULL`, a seed will be generated between 0 and the
  maximum integer number available)

- verbose:

  Write the log information from the SLiM run to the console (default
  `FALSE`)?

- debug:

  Write msprime's debug log to the console (default `FALSE`)?

- run:

  Should the msprime engine be run? If `FALSE`, the command line msprime
  command will be printed (and returned invisibly as a character vector)
  but not executed.

- path:

  Path to the directory where simulation result files will be saved. If
  `NULL`, this directory will be automatically created as a temporary
  directory. If `TRUE`, this path will be also returned by the function.
  If a string is given, it is assumed to be a path to a directory where
  simulation results will be saved. In this case, the function will
  return this path invisibly. Note that if a tree-sequence file should
  be simulated (along with other files, potentially), that tree-sequence
  file (named 'msprime.trees' by default) will have to be explicitly
  loaded using
  [`ts_read()`](https://bodkan.net/slendr/reference/ts_read.md).

- coalescent_only:

  Default is `TRUE`, which will only record the minimum amount of
  information necessary to represent the genealogical history of the
  simulated samples (i.e., only nodes which are MRCA of some pair of
  samples at some locus in the genome). Setting to `FALSE` will record
  much more information, resulting in unary nodes in the tree sequence.
  This parameter translates to the `coalescing_segments_only` argument
  of the underlying msprime method `sim_ancestry`. See Details for
  additional information.

## Value

A tree-sequence object loaded via Python-R reticulate interface function
`ts_read` (internally represented by the Python object
`tskit.trees.TreeSequence`). If the `path` argument was set, it will
return the path as a single-element character vector.

## Details

For more information about the `coalescent_only` argument, please see
msprime documentation, particularly the section on "Recording more
information" and the `coalescing_segments_only` argument of the method
`sim_ancestry()` here
<https://tskit.dev/msprime/docs/stable/ancestry.html#recording-more-information>.
and
<https://tskit.dev/msprime/docs/stable/api.html#msprime.sim_ancestry>.

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# afr and eur objects would normally be created before slendr model compilation,
# but here we take them out of the model object already compiled for this
# example (in a standard slendr simulation pipeline, this wouldn't be necessary)
afr <- model$populations[["AFR"]]
eur <- model$populations[["EUR"]]
chimp <- model$populations[["CH"]]

# schedule the sampling of a couple of ancient and present-day individuals
# given model at 20 ky, 10 ky, 5ky ago and at present-day (time 0)
modern_samples <- schedule_sampling(model, times = 0, list(afr, 10), list(eur, 100), list(chimp, 1))
ancient_samples <- schedule_sampling(model, times = c(40000, 30000, 20000, 10000), list(eur, 1))

# sampling schedules are just data frames and can be merged easily
samples <- rbind(modern_samples, ancient_samples)

# run a simulation using the msprime back end from a compiled slendr model object
ts <- msprime(model, sequence_length = 1e5, recombination_rate = 0, samples = samples)

# simulated tree-sequence object can be saved to a file using ts_write()...
ts_file <- normalizePath(tempfile(fileext = ".trees"), winslash = "/", mustWork = FALSE)
ts_write(ts, ts_file)
# ... and, at a later point, loaded by ts_read()
ts <- ts_read(ts_file, model)

summary(ts)
#> ╔═══════════════════════════╗
#> ║TreeSequence               ║
#> ╠═══════════════╤═══════════╣
#> ║Trees          │          1║
#> ╟───────────────┼───────────╢
#> ║Sequence Length│    100,000║
#> ╟───────────────┼───────────╢
#> ║Time Units     │generations║
#> ╟───────────────┼───────────╢
#> ║Sample Nodes   │        230║
#> ╟───────────────┼───────────╢
#> ║Total Size     │   39.3 KiB║
#> ╚═══════════════╧═══════════╝
#> ╔═══════════╤════╤═════════╤════════════╗
#> ║Table      │Rows│Size     │Has Metadata║
#> ╠═══════════╪════╪═════════╪════════════╣
#> ║Edges      │ 470│ 14.7 KiB│          No║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Individuals│ 115│  3.2 KiB│          No║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Migrations │   0│  8 Bytes│          No║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Mutations  │   0│ 16 Bytes│          No║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Nodes      │ 471│ 12.9 KiB│          No║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Populations│   4│338 Bytes│         Yes║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Provenances│   1│  2.9 KiB│          No║
#> ╟───────────┼────┼─────────┼────────────╢
#> ║Sites      │   0│ 16 Bytes│          No║
#> ╚═══════════╧════╧═════════╧════════════╝
#> 
```
