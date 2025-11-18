# Check that the active Python environment is setup for slendr

This function inspects the Python environment which has been activated
by the reticulate package and prints the versions of all slendr Python
dependencies to the console.

## Usage

``` r
check_env(verbose = TRUE)
```

## Arguments

- verbose:

  Should a log message be printed? If `FALSE`, only a logical value is
  returned (invisibly).

## Value

Either `TRUE` (slendr Python environment is present) or `FALSE` (slendr
Python environment is not present).

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.
check_env()
#> Summary of the currently active Python environment:
#> 
#> Python binary: /Users/mp/Library/r-miniconda-arm64/envs/Python-3.13_msprime-1.3.4_tskit-0.6.4_pyslim-1.1.0_tspop-0.0.2/bin/python 
#> Python version: 3.13.9 | packaged by conda-forge | (main, Oct 22 2025, 23:38:18) [Clang 19.1.7 ] 
#> 
#> slendr requirements:
#>  - tskit: version 0.6.4 ✓ 
#>  - msprime: version 1.3.4 ✓ 
#>  - pyslim: version 1.1.0 ✓ 
#>  - tspop: present ✓ 
```
