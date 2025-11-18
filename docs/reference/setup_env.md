# Setup a dedicated Python virtual environment for slendr

This function will automatically download a Python miniconda
distribution dedicated to an R-Python interface. It will also create a
slendr-specific Python environment with all the required Python
dependencies.

## Usage

``` r
setup_env(quiet = FALSE, agree = FALSE, pip = FALSE)
```

## Arguments

- quiet:

  Should informative messages be printed to the console? Default is
  `FALSE`.

- agree:

  Automatically agree to all questions?

- pip:

  Should pip be used instead of conda for installing slendr's Python
  dependencies?

## Value

No return value, called for side effects
