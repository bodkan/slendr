# Generate the difference between two `slendr` objects

Generate the difference between two `slendr` objects

## Usage

``` r
subtract(x, y, name = NULL)
```

## Arguments

- x:

  Object of the class `slendr`

- y:

  Object of the class `slendr`

- name:

  Optional name of the resulting geographic region. If missing, name
  will be constructed from the function arguments.

## Value

Object of the class `slendr_region` which encodes a standard spatial
object of the class `sf` with several additional attributes (most
importantly a corresponding `slendr_map` object, if applicable).
