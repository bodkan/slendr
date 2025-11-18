# Calculate the area covered by the given slendr object

Calculate the area covered by the given slendr object

## Usage

``` r
area(x)
```

## Arguments

- x:

  Object of the class `slendr`

## Value

Area covered by the input object. If a `slendr_pop` was given, a table
with an population range area in each time point will be returned. If a
`slendr_region` or `slendr_world` object was specified, the total area
covered by this object's spatial boundary will be returned.

## Examples

``` r
region_a <- region("A", center = c(20, 50), radius = 20)
region_b <- region("B", polygon = list(c(50, 40), c(70, 40), c(70, 60), c(50, 60)))
plot_map(region_a, region_b)


# note that area won't be *exactly* equal to pi*r^2:
#   https://stackoverflow.com/a/65280376
area(region_a)
#> [1] 1256.063

area(region_b)
#> [1] 400
```
