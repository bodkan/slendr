#' Take a list of all population regions and intersect them with the
#' set of underlying world map
intersect_features <- function(pop) {
  world <- attr(pop, "world")
  region <- attr(pop, "region")

  intersected <- pop

  # restrict the population range to the landscape features
  if (attr(pop, "intersect"))
    intersected <- sf::st_intersection(pop, sf::st_geometry(world))

  # restrict further to the geographic boundary, if given
  if (!is.null(region)) {
    sf::st_agr(intersected) <- "constant"
    intersected <- sf::st_intersection(intersected, sf::st_geometry(region))
  }

  sf::st_agr(intersected) <- "constant"

  # add a small tag signifying that the ranges have been processed
  # and intersected over the map of the world
  attr(intersected, "intersected") <- TRUE
  # add back the world attribute
  attr(intersected, "world") <- world

  class(intersected) <- set_class(intersected, "pop")
  intersected
}


#' Define a range (simple geometry object) for a population or a
#' geographic region
spatial_range <- function(world, center = NULL, radius = NULL, coords = NULL) {
  # check function arguments
  if (!is.null(center) & !is.null(coords))
    stop("Either a circular range (center and radius) or the corners of a polygon need to be specified, not both.", call. = F)
  if (!is.null(center) & is.null(radius))
    stop("Missing radius argument when defining a circular population range", call. = F)
  if (!is.null(coords) & length(coords) < 3)
    stop("Polygon range needs to have at least three corners")

  # circular population range or polygon range?
  if (!is.null(center)) {
    # point in the WGS-84 geographic CRS
    point_wgs84 <- sf::st_sfc(sf::st_point(center), crs = "EPSG:4326")

    # transform into target CRS
    point <- sf::st_transform(point_wgs84, sf::st_crs(world))
    # expand range into desired radius
    range <- sf::st_buffer(point, radius * 1000)
  } else {
    range <- sf::st_geometry(create_polygon(coords, world)) %>%
      sf::st_transform(crs = sf::st_crs(world))
  }

  range
}


#' Create a simple geometry polygon object from the list of
#' coordinates
create_polygon <- function(coords, world) {
  # "loop-back" to the last point to close the polygon
  coords <- c(coords, coords[1])
  coords_mat <- do.call(rbind, coords)

  # polygon in the WGS-84 geographic CRS
  polygon <-
    sf::st_polygon(list(coords_mat)) %>%
    sf::st_sfc(crs = "EPSG:4326") %>%
    sf::st_sf(geometry = .)

  polygon
}


#' Define a rectangular region for zooming in on a part of the world
define_zoom <- function(lon, lat, source_crs = "EPSG:4326") {
  x1 <- lon[1]; x2 <- lon[2]
  y1 <- lat[1]; y2 <- lat[2]
  sf::st_sfc(sf::st_polygon(list(cbind(
    c(x1, x2, x2, x1, x1),
    c(y1, y1, y2, y2, y1)
  ))), crs = source_crs)
}


#' Check whether given population region has not yet been intersected
check_not_intersected <- function(pop) {
  if (!is.null(attr(pop, "intersected")))
    stop("An already intersected population range object was provided.
Please provide a range object before it was intersected against a map.",
         call. = FALSE)
}


#' Set spannr classes (or fix their priorities if already present)
set_class <- function(x, type) {
  other_classes <- class(x) %>% .[!grepl("^spannr", .)]
  c("spannr", paste0("spannr_", type), other_classes)
}
