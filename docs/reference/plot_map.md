# Plot `slendr` geographic features on a map

Plots objects of the three `slendr` spatial classes (`slendr_map`,
`slendr_region`, and `slendr_pop`).

## Usage

``` r
plot_map(
  ...,
  time = NULL,
  gene_flow = FALSE,
  splits = FALSE,
  labels = FALSE,
  arrows = TRUE,
  graticules = "original",
  intersect = TRUE,
  show_map = TRUE,
  title = NULL,
  interpolated_maps = NULL
)
```

## Arguments

- ...:

  Objects of classes `slendr_map`, `slendr_region`, or `slendr_pop`

- time:

  Plot a concrete time point

- gene_flow:

  Indicate gene-flow events by linking demes with a line

- splits:

  Indicate split events with lines

- labels:

  Should the (starting) polygons of each populations be labeled with a
  respective population label (default `FALSE`)?

- arrows:

  Should gene-flow links be also indicated with an arrow?

- graticules:

  Plot graticules in the original Coordinate Reference System (such as
  longitude-latitude), or in the internal CRS (such as meters)?

- intersect:

  Intersect the population boundaries against landscape and other
  geographic boundaries (default `TRUE`)?

- show_map:

  Show the underlying world map

- title:

  Title of the plot

- interpolated_maps:

  Interpolated spatial boundaries for all populations in all time points
  (this is only used for plotting using the `explore` shiny app)

## Value

A ggplot2 object with the visualized slendr map
