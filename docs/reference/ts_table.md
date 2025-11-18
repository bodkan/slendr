# Get the table of individuals/nodes/edges/mutations/sites from the tree sequence

This function extracts data from a given tree sequence table. All times
are converted to model-specific time units from tskit's "generations
backwards" time direction.

## Usage

``` r
ts_table(ts, table = c("individuals", "edges", "nodes", "mutations", "sites"))
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- table:

  Which tree sequence table to return

## Value

Data frame with the information from the give tree-sequence table (can
be either a table of individuals, edges, nodes, or mutations).

## Details

For further processing and analyses, the output of the function
[`ts_nodes`](https://slendr.net/reference/ts_nodes.md) might be more
useful, as it merges the information in node and individual tables into
one table and further annotates it with useful information from the
model configuration data.

## See also

[`ts_nodes`](https://slendr.net/reference/ts_nodes.md) and
[`ts_edges`](https://slendr.net/reference/ts_edges.md) for accessing an
annotated, more user-friendly and analysis-friendly tree-sequence table
data

## Examples

``` r
# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk and add mutations to it
ts <- ts_read(slendr_ts, model) %>% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

# get the 'raw' tskit table of individuals
ts_table(ts, "individuals")
#> # A tibble: 71 × 9
#>    ind_id  time pedigree_id  pop_id alive remembered retained sampled time_tskit
#>     <dbl> <dbl>       <dbl> <int[1> <lgl> <lgl>      <lgl>    <lgl>    <dbl[1d]>
#>  1      0   -10    16023346       0 TRUE  TRUE       TRUE     TRUE             0
#>  2      1   -10    16023350       1 TRUE  TRUE       TRUE     TRUE             0
#>  3      2   -10    16023352       1 TRUE  TRUE       TRUE     TRUE             0
#>  4      3   -10    16023353       1 TRUE  TRUE       TRUE     TRUE             0
#>  5      4   -10    16023355       1 TRUE  TRUE       TRUE     TRUE             0
#>  6      5   -10    16023359       1 TRUE  TRUE       TRUE     TRUE             0
#>  7      6   -10    16024625       3 TRUE  TRUE       TRUE     TRUE             0
#>  8      7   -10    16025081       3 TRUE  TRUE       TRUE     TRUE             0
#>  9      8   -10    16025520       3 TRUE  TRUE       TRUE     TRUE             0
#> 10      9   -10    16026684       3 TRUE  TRUE       TRUE     TRUE             0
#> # ℹ 61 more rows

# get the 'raw' tskit table of edges
ts_table(ts, "edges")
#> # A tibble: 294 × 5
#>       id child parent   left  right
#>    <dbl> <int>  <int>  <dbl>  <dbl>
#>  1     0     6     26      0 500000
#>  2     1    13     26      0 500000
#>  3     2     7     27      0 500000
#>  4     3     8     27      0 500000
#>  5     4    12     27      0 500000
#>  6     5     5     28      0 500000
#>  7     6    10     28      0 500000
#>  8     7    11     29 306056 500000
#>  9     8    26     29 306056 500000
#> 10     9    27     30      0 500000
#> # ℹ 284 more rows

# get the 'raw' tskit table of nodes
ts_table(ts, "nodes")
#> # A tibble: 85 × 5
#>    node_id    ind_id pop_id  time time_tskit
#>      <int> <int[1d]>  <int> <dbl>      <dbl>
#>  1       0        24      2 70010       2334
#>  2       1        24      2 70010       2334
#>  3       2        41      2 40010       1334
#>  4       3        41      2 40010       1334
#>  5       4         1      1   -10          0
#>  6       5         1      1   -10          0
#>  7       6         2      1   -10          0
#>  8       7         2      1   -10          0
#>  9       8         3      1   -10          0
#> 10       9         3      1   -10          0
#> # ℹ 75 more rows

# get the 'raw' tskit table of mutations
ts_table(ts, "mutations")
#> # A tibble: 2,213 × 5
#>       id  site  node     time time_tskit
#>    <dbl> <int> <int>    <dbl>      <dbl>
#>  1     0     0    81  588320.     19611.
#>  2     1     1    82 5259704.    175324.
#>  3     2     2    82  757010.     25234.
#>  4     3     3    81  241423.      8048.
#>  5     4     4    32 3622571.    120753.
#>  6     5     5    32  104626.      3488.
#>  7     6     6    82 5179605.    172654.
#>  8     7     7    82 5212483.    173750.
#>  9     8     8    82 2373328.     79111.
#> 10     9     9    32 1159112.     38637.
#> # ℹ 2,203 more rows

# get the 'raw' tskit table of sites
ts_table(ts, "sites")
#> # A tibble: 2,210 × 3
#>       id position ancestral_state
#>    <dbl>    <dbl> <chr>          
#>  1     0      572 C              
#>  2     1      782 T              
#>  3     2     1609 C              
#>  4     3     2033 G              
#>  5     4     2316 A              
#>  6     5     2469 T              
#>  7     6     2614 G              
#>  8     7     2752 T              
#>  9     8     2879 A              
#> 10     9     3192 G              
#> # ℹ 2,200 more rows
```
