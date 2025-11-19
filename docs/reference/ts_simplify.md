# Simplify the tree sequence down to a given set of individuals

This function is a convenience wrapper around the `simplify` method
implemented in tskit, designed to work on tree sequence data simulated
by SLiM using the slendr R package.

## Usage

``` r
ts_simplify(
  ts,
  simplify_to = NULL,
  keep_input_roots = FALSE,
  keep_unary = FALSE,
  keep_unary_in_individuals = FALSE,
  filter_nodes = TRUE
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- simplify_to:

  A character vector of individual names. If NULL, all explicitly
  remembered individuals (i.e. those specified via the
  [`schedule_sampling`](https://bodkan.net/slendr/reference/schedule_sampling.md)
  function will be left in the tree sequence after the simplification.

- keep_input_roots:

  Should the history ancestral to the MRCA of all samples be retained in
  the tree sequence? Default is `FALSE`.

- keep_unary:

  Should unary nodes be preserved through simplification? Default is
  `FALSE`.

- keep_unary_in_individuals:

  Should unary nodes be preserved through simplification if they are
  associated with an individual recorded in the table of individuals?
  Default is `FALSE`. Cannot be set to `TRUE` if `keep_unary` is also
  TRUE

- filter_nodes:

  Should nodes be reindexed after simplification? Default is `TRUE`. See
  tskit's documentation for the Python method `simplify()`

## Value

Tree-sequence object of the class `slendr_ts`, which serves as an
interface point for the Python module tskit using slendr functions with
the `ts_` prefix.

## Details

The simplification process is used to remove redundant information from
the tree sequence and retains only information necessary to describe the
genealogical history of a set of samples.

For more information on how simplification works in pyslim and tskit,
see the official documentation at
<https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.simplify>
and <https://tskit.dev/pyslim/docs/latest/tutorial.html#simplification>.

A very clear description of the difference between remembering and
retaining and how to use these techniques to implement historical
individuals (i.e. ancient DNA samples) is in the pyslim documentation at
<https://tskit.dev/pyslim/docs/latest/tutorial.html#historical-individuals>.

## See also

[`ts_nodes`](https://bodkan.net/slendr/reference/ts_nodes.md) for extracting
useful information about individuals, nodes, coalescent times and
geospatial locations of nodes on a map

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

ts <- ts_read(slendr_ts, model)
ts
#> ╔════════════════════════╗
#> ║TreeSequence            ║
#> ╠═══════════════╤════════╣
#> ║Trees          │      68║
#> ╟───────────────┼────────╢
#> ║Sequence Length│ 500,000║
#> ╟───────────────┼────────╢
#> ║Time Units     │   ticks║
#> ╟───────────────┼────────╢
#> ║Sample Nodes   │      26║
#> ╟───────────────┼────────╢
#> ║Total Size     │79.6 KiB║
#> ╚═══════════════╧════════╝
#> ╔═══════════╤════╤════════╤════════════╗
#> ║Table      │Rows│Size    │Has Metadata║
#> ╠═══════════╪════╪════════╪════════════╣
#> ║Edges      │ 294│ 9.2 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Individuals│  71│ 8.7 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Migrations │   0│ 8 Bytes│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Mutations  │   0│ 1.2 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Nodes      │  85│ 4.7 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Populations│   5│ 2.6 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Provenances│   3│46.2 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Sites      │   0│16 Bytes│          No║
#> ╚═══════════╧════╧════════╧════════════╝
#> 

# simplify tree sequence to sampled individuals
ts_simplified <- ts_simplify(ts)

# simplify to a subset of sampled individuals
ts_small <- ts_simplify(ts, simplify_to = c("CH_1", "NEA_1", "NEA_2", "AFR_1",
                                            "AFR_2", "EUR_1", "EUR_2"))

ts_small
#> ╔════════════════════════╗
#> ║TreeSequence            ║
#> ╠═══════════════╤════════╣
#> ║Trees          │      27║
#> ╟───────────────┼────────╢
#> ║Sequence Length│ 500,000║
#> ╟───────────────┼────────╢
#> ║Time Units     │   ticks║
#> ╟───────────────┼────────╢
#> ║Sample Nodes   │      14║
#> ╟───────────────┼────────╢
#> ║Total Size     │68.0 KiB║
#> ╚═══════════════╧════════╝
#> ╔═══════════╤════╤════════╤════════════╗
#> ║Table      │Rows│Size    │Has Metadata║
#> ╠═══════════╪════╪════════╪════════════╣
#> ║Edges      │ 122│ 3.8 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Individuals│  33│ 5.0 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Migrations │   0│ 8 Bytes│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Mutations  │   0│ 1.2 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Nodes      │  40│ 3.1 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Populations│   5│ 2.6 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Provenances│   4│46.7 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Sites      │   0│16 Bytes│          No║
#> ╚═══════════╧════╧════════╧════════════╝
#> 
```
