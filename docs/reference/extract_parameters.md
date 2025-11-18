# Extract information from a compiled model or a simulated tree sequence

This function extract a slendr model parameters used to compile a given
model object or simulate a tree sequence

## Usage

``` r
extract_parameters(data)
```

## Arguments

- data:

  Either an object of the class `slendr_ts` or `slendr_model`

## Value

A list of data frames containing parameters of the model used when
compiling a model object

## Examples

``` r
init_env()
#> The interface to all required Python modules has been activated.

# load an example model and simulate a tree sequence from it
model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))
ts <- msprime(model, sequence_length = 1e5, recombination_rate = 0)

# extract model parameters from a compiled model object as a list of data frames
extract_parameters(model)
#> $splits
#>   pop parent    N    time remove
#> 1  CH   <NA>   10 6500000     NA
#> 2 AFR     CH   10 6000000     NA
#> 3 NEA    AFR   10  600000  40000
#> 4 EUR    AFR 5000   70000     NA
#> 
#> $gene_flows
#>   from  to start   end rate
#> 1  NEA EUR 55000 45000 0.03
#> 

# the function can also extract parameters of a model which simulated a
# tree sequence
extract_parameters(ts)
#> $splits
#>   pop parent    N    time remove
#> 1  CH   <NA>   10 6500000     NA
#> 2 AFR     CH   10 6000000     NA
#> 3 NEA    AFR   10  600000  40000
#> 4 EUR    AFR 5000   70000     NA
#> 
#> $gene_flows
#>   from  to start   end rate
#> 1  NEA EUR 55000 45000 0.03
#> 
```
