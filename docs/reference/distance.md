# Calculate the distance between a pair of spatial boundaries

Calculate the distance between a pair of spatial boundaries

## Usage

``` r
distance(x, y, measure, time = NULL)
```

## Arguments

- x, y:

  Objects of the class `slendr`

- measure:

  How to measure distance? This can be either `'border'` (distance
  between the borders of `x` and `y`) or `'center'` (distance between
  their centroids).

- time:

  Time closest to the spatial maps of `x` and `y` if they represent
  `slendr_pop` population boundaries (ignored for general
  `slendr_region` objects)

## Value

If the coordinate reference system was specified, a distance in
projected units (i.e. meters) is returned. Otherwise the function
returns a normal Euclidean distance.

## Examples

``` r
# create two regions on a blank abstract landscape
region_a <- region("A", center = c(20, 50), radius = 20)
region_b <- region("B", center = c(80, 50), radius = 20)
plot_map(region_a, region_b)


# compute the distance between the centers of both population ranges
distance(region_a, region_b, measure = "center")
#> [1] 60

# compute the distance between the borders of both population ranges
distance(region_a, region_b, measure = "border")
#> [1] 20
```
