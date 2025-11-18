# Substitute values of parameters in a SLiM extension template

Substitute values of templated {{parameters}} in a given SLiM extension
template

## Usage

``` r
substitute_values(template, ...)
```

## Arguments

- template:

  Either a path to an extension script file, or a string containing the
  entire SLiM extension code

- ...:

  Named function arguments interpreted as key=value pairs to be used in
  argument substitution

## Value

Path to a file with a saved extension script containing all substituted
values

## Details

If a file or a multi-line string given as `template` contains parameters
specified as {{param}} where "param" can be arbitrary variable name,
this function substitutes each templated {{parameter}} for a given
values. Such modified template is then used to extend a built-in slendr
SLiM script, allowing for a customization of its default behavior (most
commonly replacing its assumption of neutrality for non-neutral
scenarios, such as simulations of natural selection).
