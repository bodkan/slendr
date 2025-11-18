# Check that the required dependencies are available for slendr to work

Check that the required dependencies are available for slendr to work

## Usage

``` r
check_dependencies(python = FALSE, slim = FALSE, quit = FALSE)
```

## Arguments

- python:

  Is the slendr Python environment required?

- slim:

  Is SLiM required?

- quit:

  Should the R interpreter quit if required slendr dependencies are
  missing? This option (which is not turned on by default, being set to
  `FALSE`) is used mainly in avoiding running slendr man page examples
  on machines which lack dependencies. If set to `TRUE`, a logical value
  is returned.

## Value

If `quit = TRUE`, no values is returned, if `quit = FALSE`, a scalar
logical value is returned indicating whether or not the dependencies are
present.
