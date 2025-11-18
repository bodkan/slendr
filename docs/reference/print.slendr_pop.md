# Print a short summary of a `slendr` object

All spatial objects in the slendr package are internally represented as
Simple Features (`sf`) objects. This fact is hidden in most
circumstances this, as the goal of the slendr package is to provide
functionality at a much higher level (population boundaries, geographic
regions, instead of individual polygons and other "low-level" geometric
objects), without the users having to worry about low-level details
involved in handling spatial geometries. However, the full `sf` object
representation can be always printed by calling `x[]`.

## Usage

``` r
# S3 method for class 'slendr_pop'
print(x, ...)

# S3 method for class 'slendr_region'
print(x, ...)

# S3 method for class 'slendr_map'
print(x, ...)

# S3 method for class 'slendr_model'
print(x, ...)
```

## Arguments

- x:

  Object of a class `slendr` (either `slendr_pop`, `slendr_map`,
  `slendr_region`, or `slendr_table`)

- ...:

  Additional arguments passed to `print`

## Value

No return value, used only for printing
