# Calculate the f2, f3, f4, and f4-ratio statistics

These functions present an R interface to the corresponding f-statistics
methods in tskit.

## Usage

``` r
ts_f2(
  ts,
  A,
  B,
  mode = c("site", "branch", "node"),
  span_normalise = TRUE,
  windows = NULL
)

ts_f3(
  ts,
  A,
  B,
  C,
  mode = c("site", "branch", "node"),
  span_normalise = TRUE,
  windows = NULL
)

ts_f4(
  ts,
  W,
  X,
  Y,
  Z,
  mode = c("site", "branch", "node"),
  span_normalise = TRUE,
  windows = NULL
)

ts_f4ratio(
  ts,
  X,
  A,
  B,
  C,
  O,
  mode = c("site", "branch"),
  span_normalise = TRUE
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- mode:

  The mode for the calculation ("sites" or "branch")

- span_normalise:

  Divide the result by the span of the window? Default TRUE, see the
  tskit documentation for more detail.

- windows:

  Coordinates of breakpoints between windows. The first coordinate (0)
  and the last coordinate (equal to `ts$sequence_length`) do not have to
  be specified as they are added automatically.

- W, X, Y, Z, A, B, C, O:

  Character vectors of individual names (largely following the
  nomenclature of Patterson 2021, but see crucial differences between
  tskit and ADMIXTOOLS in Details)

## Value

Data frame with statistics calculated for the given sets of individuals

## Details

Note that the order of populations f3 statistic implemented in tskit
(<https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.f3>)
is different from what you might expect from ADMIXTOOLS, as defined in
Patterson 2012 (see
[doi:10.1534/genetics.112.145037](https://doi.org/10.1534/genetics.112.145037)
under heading "The three-population test and introduction of
f-statistics", as well as ADMIXTOOLS documentation at
<https://github.com/DReichLab/AdmixTools/blob/master/README.3PopTest#L5>).
Specifically, the widely used notation introduced by Patterson assumes
the population triplet as f3(C; A, B), with C being the "focal" sample
(i.e., either the outgroup or a sample tested for admixture). In
contrast, tskit implements f3(A; B, C), with the "focal sample" being A.

Although this is likely to confuse many ADMIXTOOLS users, slendr does
not have much choice in this, because its `ts_*()` functions are
designed to be broadly compatible with raw tskit methods.

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk and add mutations to it
ts <- ts_read(slendr_ts, model) %>% ts_mutate(mutation_rate = 1e-8, random_seed = 42)

# calculate f2 for two individuals in a previously loaded tree sequence
ts_f2(ts, A = "AFR_1", B = "EUR_1")
#> # A tibble: 1 × 3
#>   A     B           f2
#>   <chr> <chr>    <dbl>
#> 1 AFR_1 EUR_1 0.000028

# calculate f2 for two sets of individuals
ts_f2(ts, A = c("AFR_1", "AFR_2"), B = c("EUR_1", "EUR_3"))
#> # A tibble: 1 × 3
#>   A           B                  f2
#>   <chr>       <chr>           <dbl>
#> 1 AFR_1+AFR_2 EUR_1+EUR_3 0.0000307

# calculate f3 for two individuals in a previously loaded tree sequence
ts_f3(ts, A = "EUR_1", B = "AFR_1", C = "NEA_1")
#> # A tibble: 1 × 4
#>   A     B     C            f3
#>   <chr> <chr> <chr>     <dbl>
#> 1 EUR_1 AFR_1 NEA_1 -0.000019

# calculate f3 for two sets of individuals
ts_f3(ts, A = c("AFR_1", "AFR_2", "EUR_1", "EUR_2"),
          B = c("NEA_1", "NEA_2"),
          C = "CH_1")
#> # A tibble: 1 × 4
#>   A                       B           C           f3
#>   <chr>                   <chr>       <chr>    <dbl>
#> 1 AFR_1+AFR_2+EUR_1+EUR_2 NEA_1+NEA_2 CH_1  0.000157

# calculate f4 for single individuals
ts_f4(ts, W = "EUR_1", X = "AFR_1", Y = "NEA_1", Z = "CH_1")
#> # A tibble: 1 × 5
#>   W     X     Y     Z           f4
#>   <chr> <chr> <chr> <chr>    <dbl>
#> 1 EUR_1 AFR_1 NEA_1 CH_1  0.000011

# calculate f4 for sets of individuals
ts_f4(ts, W = c("EUR_1", "EUR_2"),
          X = c("AFR_1", "AFR_2"),
          Y = "NEA_1",
          Z = "CH_1")
#> # A tibble: 1 × 5
#>   W           X           Y     Z            f4
#>   <chr>       <chr>       <chr> <chr>     <dbl>
#> 1 EUR_1+EUR_2 AFR_1+AFR_2 NEA_1 CH_1  0.0000205

# calculate f4-ratio for a given set of target individuals X
ts_f4ratio(ts, X = c("EUR_1", "EUR_2", "EUR_4", "EUR_5"),
               A = "NEA_1", B = "NEA_2", C = "AFR_1", O = "CH_1")
#> # A tibble: 4 × 6
#>   X     A     B     C     O      alpha
#>   <chr> <chr> <chr> <chr> <chr>  <dbl>
#> 1 EUR_1 NEA_1 NEA_2 AFR_1 CH_1  0.0625
#> 2 EUR_2 NEA_1 NEA_2 AFR_1 CH_1  0.170 
#> 3 EUR_4 NEA_1 NEA_2 AFR_1 CH_1  0.0455
#> 4 EUR_5 NEA_1 NEA_2 AFR_1 CH_1  0     
```
