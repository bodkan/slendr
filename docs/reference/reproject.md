# Reproject coordinates between coordinate systems

Converts between coordinates on a compiled raster map (i.e. pixel units)
and different Geographic Coordinate Systems (CRS).

## Usage

``` r
reproject(
  from,
  to,
  x = NULL,
  y = NULL,
  coords = NULL,
  model = NULL,
  add = FALSE,
  input_prefix = "",
  output_prefix = "new"
)
```

## Arguments

- from, to:

  Either a CRS code accepted by GDAL, a valid integer EPSG value, an
  object of class `crs`, the value "raster" (converting from/to pixel
  coordinates), or "world" (converting from/to whatever CRS is set for
  the underlying map)

- x, y:

  Coordinates in two dimensions (if missing, coordinates are expected to
  be in the `data.frame` specified in the `coords` parameter as columns
  "x" and "y")

- coords:

  data.frame-like object with coordinates in columns "x" and "y"

- model:

  Object of the class `slendr_model`

- add:

  Add column coordinates to the input data.frame `coords` (coordinates
  otherwise returned as a separate object)?

- input_prefix, output_prefix:

  Input and output prefixes of data frame columns with spatial
  coordinates

## Value

Data.frame with converted two-dimensional coordinates given as input

## Examples

``` r
lon_lat_df <- data.frame(x = c(30, 0, 15), y = c(60, 40, 10))

reproject(
  from = "epsg:4326",
  to = "epsg:3035",
  coords = lon_lat_df,
  add = TRUE # add converted [lon,lat] coordinates as a new column
)
#> # A tibble: 3 × 4
#>       x     y     newx      newy
#>   <dbl> <dbl>    <dbl>     <dbl>
#> 1    30    60 5422493.  4256803.
#> 2     0    40 3465349.  1934879.
#> 3    15    10 4907297. -1328914.
```
