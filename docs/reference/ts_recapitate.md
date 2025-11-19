# Recapitate the tree sequence

Recapitate the tree sequence

## Usage

``` r
ts_recapitate(
  ts,
  recombination_rate,
  Ne = NULL,
  demography = NULL,
  random_seed = NULL
)
```

## Arguments

- ts:

  Tree sequence object loaded by `ts_read`

- recombination_rate:

  A constant value of the recombination rate

- Ne:

  Effective population size during the recapitation process

- demography:

  Ancestral demography to be passed internally to
  `msprime.sim_ancestry()` (see msprime's documentation for mode detail)

- random_seed:

  Random seed passed to pyslim's `recapitate` method (if `NULL`, a seed
  will be generated between 0 and the maximum integer number available)

## Value

Tree-sequence object of the class `slendr_ts`, which serves as an
interface point for the Python module tskit using slendr functions with
the `ts_` prefix.

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

ts <- ts_read(slendr_ts, model) %>%
  ts_recapitate(recombination_rate = 1e-8, Ne = 10000, random_seed = 42)

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
#> ║Total Size     │82.1 KiB║
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
#> ║Populations│   6│ 2.7 KiB│         Yes║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Provenances│   4│48.7 KiB│          No║
#> ╟───────────┼────┼────────┼────────────╢
#> ║Sites      │   0│16 Bytes│          No║
#> ╚═══════════╧════╧════════╧════════════╝
#> 
```
