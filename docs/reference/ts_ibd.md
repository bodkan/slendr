# Collect Identity-by-Descent (IBD) segments (EXPERIMENTAL)

This function iterates over a tree sequence and returns IBD tracts
between pairs of individuals or nodes

## Usage

``` r
ts_ibd(
  ts,
  coordinates = FALSE,
  within = NULL,
  between = NULL,
  squash = FALSE,
  minimum_length = NULL,
  maximum_time = NULL,
  sf = TRUE
)
```

## Arguments

- ts:

  Tree sequence object of the class `slendr_ts`

- coordinates:

  Should coordinates of all detected IBD tracts be reported? If `FALSE`
  (the default), only the total length of shared IBD segments and their
  numbers are reported. If `TRUE`, coordinates of each segment will be
  returned (but note that this can have a massive impact on memory
  usage). See details for more information.

- within:

  A character vector with individual names or an integer vector with
  node IDs indicating a set of nodes within which to look for IBD
  segments.

- between:

  A list of lists of character vectors with individual names or integer
  vectors with node IDs, indicating a set of nodes between which to look
  for shared IBD segments.

- squash:

  Should adjacent IBD segments for pairs of nodes be squashed if they
  only differ by their 'genealogical paths' but not by their MRCA?
  Default is `FALSE`. For more context, see
  <https://github.com/tskit-dev/tskit/issues/2459>. This option is
  EXPERIMENTAL!

- minimum_length:

  Minimum length of an IBD segment to return in results. This is useful
  for reducing the total amount of IBD returned (but see Details).

- maximum_time:

  Oldest MRCA of a node to be considered as an IBD ancestor to return
  that IBD segment in results. This is useful for reducing the total
  amount of IBD returned.

- sf:

  If IBD segments in a spatial tree sequence are being analyzed, should
  the returned table be a spatial sf object? Default is `TRUE`.

## Value

A data frame with IBD results (either coordinates of each IBD segment
shared by a pair of nodes, or summary statistics about the total IBD
sharing for that pair)

## Details

This function is considered experimental. For full control over IBD
segment detection in tree-sequence data, users can (and perhaps, for the
time being, should) rely on the tskit method `ibd_segments` (see
<https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.ibd_segments>).

Iternally, this function leverages the tskit `TreeSequence` method
`ibd_segments`. However, note that the `ts_ibd` function always returns
a data frame of IBD tracts, it does not provide an option to iterate
over individual IBD segments as shown in the official tskit
documentation at <https://tskit.dev/tskit/docs/stable/ibd.html>. In
general, R handles heavy iteration poorly, and this function does not
attempt to serve as a full wrapper to `ibd_segments`.

Unfortunately, the distinction between "squashed IBD" (what many would
consider to be the expected definition of IBD) and tskit’s IBD which is
defined via distinct genealogical paths (see
<https://github.com/tskit-dev/tskit/issues/2459> for a discussion of the
topic), makes the meaning of the filtering parameter of the
`ibd_segments()` method of tskit `minimum_length` somewhat unintuitive.
As of this moment, this function argument filters on IBD segments on the
tskit level, not the level of the squashed IBD segments!

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model with an already simulated tree sequence
slendr_ts <- system.file("extdata/models/introgression_slim.trees", package = "slendr")
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))

# load the tree-sequence object from disk
ts <- ts_read(slendr_ts, model)

# find IBD segments between specified Neanderthals and Europeans
ts_ibd(
  ts,
  coordinates = TRUE,
  between = list(c("NEA_1", "NEA_2"), c("EUR_1", "EUR_2")),
  minimum_length = 40000
)
#> # A tibble: 32 × 13
#>    node1 node2 length  mrca node1_time node2_time tmrca   left  right name1
#>    <int> <int>  <dbl> <dbl>      <dbl>      <dbl> <dbl>  <dbl>  <dbl> <chr>
#>  1     0    16  87198    82      70000          0 20001  65949 153147 NEA_1
#>  2     0    16  85735    82      70000          0 20001 221193 306928 NEA_1
#>  3     0    17  68822    82      70000          0 20001  65949 134771 NEA_1
#>  4     0    17  53084    82      70000          0 20001 227724 280808 NEA_1
#>  5     0    18  87198    82      70000          0 20001  65949 153147 NEA_1
#>  6     0    18  85735    82      70000          0 20001 221193 306928 NEA_1
#>  7     0    19  81722    82      70000          0 20001  71425 153147 NEA_1
#>  8     0    19  58054    79      70000          0  2340 227185 285239 NEA_1
#>  9     1    16  87198    82      70000          0 20001  65949 153147 NEA_1
#> 10     1    16  85735    82      70000          0 20001 221193 306928 NEA_1
#> # ℹ 22 more rows
#> # ℹ 3 more variables: name2 <chr>, pop1 <fct>, pop2 <fct>
```
