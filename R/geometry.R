# Take a population region and intersect them with the
# underlying world map or its geographic region
intersect_features <- function(pop) {
  map <- attr(pop, "map")
  region <- attr(pop, "region")

  intersected <- pop

  # restrict the population range to the landscape features
  if (attr(pop, "intersect") & nrow(map)) {
    intersected <- sf::st_intersection(pop, sf::st_geometry(map))
    if (attr(pop, "aquatic"))
      intersected <- sf::st_difference(pop, sf::st_combine(intersected))
    if (any(area(intersected)$area == 0))
      stop(sprintf("No area left for %s after intersection with landscape at time %s",
                   pop$pop, pop$time), call. = FALSE)
  }

  sf::st_agr(intersected) <- "constant"

  result <- copy_attributes(
    intersected,
    pop,
    c("map", "remove", "parent", "intersect", "aquatic")
  )

  # add a small tag signifying that the ranges have been processed
  # and intersected over the map of the world
  attr(result, "intersected") <- TRUE

  result
}


# Define a range (simple geometry object) for a population or a
# geographic region
define_boundary <- function(map, center = NULL, radius = NULL, coords = NULL) {
  # check function arguments
  if (all(is.null(c(center, radius, coords))))
    stop("Either a circular range or a polygon range must be specified", call. = FALSE)
  if (!is.null(center) & !is.null(coords))
    stop("Either a circular range (center and radius) or the corners of
a polygon need to be specified, but not both", call. = FALSE)
  if (!is.null(center) & is.null(radius))
    stop("Missing radius argument when defining a circular population range", call. = FALSE)
  if (is.null(center) & !is.null(radius))
    stop("Missing center argument when defining a circular population range", call. = FALSE)
  if (!is.null(coords) & length(coords) < 3)
    stop("Polygon range needs to have at least three corners")

  # circular population range or polygon range?
  if (!is.null(center)) {
    point <- sf::st_sfc(sf::st_point(center))

    # if the map is in WGS-84, interpret the radius as in units of degrees
    # but see https://stackoverflow.com/a/70151787 for apparent visual artifacts
    # this can produce {
    if (has_crs(map)) {
      sf::st_crs(point) <- "EPSG:4326"
      if (sf::st_is_longlat(map)) {
        radius <- units::set_units(radius, arc_degree)
        warning(
          paste0("Your map is using a WGS-84 geographic coordinate system (CRS), meaning that\n",
                "radius distances are interpreted in units of degrees. Consider using\n",
                "a projected CRS in order to specify distances in meters."), call. = FALSE
        )
      }
    }

    # generate geometry of a circular range around the center point
    range <- sf::st_buffer(point, radius)
  } else {
    # generate geometry of a polygon from a given list of coordinates
    range <- sf::st_geometry(create_polygon(coords))
  }

  if (has_crs(map)) {
    sf::st_crs(range) <- "EPSG:4326"
    range <- sf::st_transform(range, sf::st_crs(map))
  }

  range
}


# Create a simple geometry polygon object from the list of
# coordinates
create_polygon <- function(coords) {
  # "loop-back" to the last point to close the polygon
  coords <- c(coords, coords[1])
  coords_mat <- do.call(rbind, coords)

  # polygon in the WGS-84 geographic CRS
  polygon <-
    sf::st_polygon(list(coords_mat)) %>%
    sf::st_sfc() %>%
    sf::st_sf(geometry = .)

  polygon
}


# Define a rectangular region for zooming in on a part of the world
define_zoom <- function(lon, lat, source_crs = "EPSG:4326") {
  x1 <- lon[1]
  x2 <- lon[2]
  y1 <- lat[1]
  y2 <- lat[2]
  sf::st_sfc(sf::st_polygon(list(cbind(
    c(x1, x2, x2, x1, x1),
    c(y1, y1, y2, y2, y1)
  ))), crs = source_crs)
}
