# Extract genotype table from the tree sequence

Extract genotype table from the tree sequence

## Usage

``` r
ts_genotypes(ts, quiet = FALSE)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- quiet:

  Should messages about multiallelic sites be silenced? Default is
  `FALSE`.

## Value

Data frame object of the class `tibble` containing genotypes of
simulated individuals in columns

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk, recapitate it, simplify it, and mutate it
ts <- ts_read(slendr_ts, model) %>%
  ts_recapitate(Ne = 10000, recombination_rate = 1e-8) %>%
  ts_simplify() %>%
  ts_mutate(mutation_rate = 1e-8)

# extract the genotype matrix (this could take  a long time consume lots
# of memory!)
gts <- ts_genotypes(ts)
#> 2 multiallelic sites (0.092% out of 2175 total) detected and removed
```
