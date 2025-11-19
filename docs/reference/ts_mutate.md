# Add mutations to the given tree sequence

Add mutations to the given tree sequence

## Usage

``` r
ts_mutate(
  ts,
  mutation_rate,
  random_seed = NULL,
  keep_existing = TRUE,
  mutation_model = NULL
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- mutation_rate:

  Mutation rate used by msprime to simulate mutations

- random_seed:

  Random seed passed to msprime's `mutate` method (if `NULL`, a seed
  will be generated between 0 and the maximum integer number available)

- keep_existing:

  Keep existing mutations?

- mutation_model:

  Which mutation model to use? If `NULL` (default), no special mutation
  type will be used. Otherwise, a mutation model matching
  <https://tskit.dev/msprime/docs/stable/mutations.html> may be provided
  as a Python/reticulate object. For instance,
  `msprime$SLiMMutationModel(type=42L)` will add SLiM mutation with the
  mutation type 42.

## Value

Tree-sequence object of the class `slendr_ts`, which serves as an
interface point for the Python module tskit using slendr functions with
the `ts_` prefix.

## See also

[`ts_nodes`](https://bodkan.net/slendr/reference/ts_nodes.md) for
extracting useful information about individuals, nodes, coalescent times
and geospatial locations of nodes on a map

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

ts <- ts_read(slendr_ts, model)
ts_mutate <- ts_mutate(ts, mutation_rate = 1e-8, random_seed = 42)

ts_mutate
#> ╔═════════════════════════╗
#> ║TreeSequence             ║
#> ╠═══════════════╤═════════╣
#> ║Trees          │       68║
#> ╟───────────────┼─────────╢
#> ║Sequence Length│  500,000║
#> ╟───────────────┼─────────╢
#> ║Time Units     │    ticks║
#> ╟───────────────┼─────────╢
#> ║Sample Nodes   │       26║
#> ╟───────────────┼─────────╢
#> ║Total Size     │214.2 KiB║
#> ╚═══════════════╧═════════╝
#> ╔═══════════╤═════╤════════╤════════════╗
#> ║Table      │Rows │Size    │Has Metadata║
#> ╠═══════════╪═════╪════════╪════════════╣
#> ║Edges      │  294│ 9.2 KiB│          No║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Individuals│   71│ 8.7 KiB│         Yes║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Migrations │    0│ 8 Bytes│          No║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Mutations  │2,213│81.2 KiB│          No║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Nodes      │   85│ 4.7 KiB│         Yes║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Populations│    5│ 2.6 KiB│         Yes║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Provenances│    4│46.9 KiB│          No║
#> ╟───────────┼─────┼────────┼────────────╢
#> ║Sites      │2,210│54.0 KiB│          No║
#> ╚═══════════╧═════╧════════╧════════════╝
#> 
```
