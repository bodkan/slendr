library(ggplot2)
library(ggspatial)
library(sf)
library(patchwork)

library(rnaturalearth)
library(rnaturalearthdata)


create_polygon <- function(coords, world, source_crs = "EPSG:4326") {
  target_crs <- sf::st_crs(world)

  ## "loop-back" to the last point to close the polygon
  coords <- c(coords, coords[1])
  coords_mat <- do.call(rbind, coords)

  ## polygon in the WGS84 geographic CRS
  polygon_wgs84 <-
    sf::st_polygon(list(coords_mat)) %>%
    sf::st_sfc(crs = source_crs) %>%
    sf::st_sf(geometry = .)

  ## transform into target CRS
  polygon <- sf::st_transform(polygon_wgs84, target_crs)

  sf::st_agr(polygon) <- "constant"

  polygon
}


make_region <- function(name, coords, world, source_crs = "EPSG:4326") {
  region <- create_polygon(coords, world, source_crs) %>%
    sf::st_sf(region = name, geometry = sf::st_geometry(.))
  ## set attributes as constant throughout the geometry
  sf::st_agr(region) <- "constant"
  region
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

#' Download Natural Earth land area map data, crop it to a given
#' boundary and transform to a specified projection CRS
#' downloaded to ~/projects/ with:
#' rnaturalearth::ne_download(scale = "small", type = "land", category = "physical", load = F, destdir = "~/projects/")
make_world <- function(lon, lat, target_crs) {
  ## load the map data (either from a cache location on disk or from
  ## the server)
  world <- rnaturalearth::ne_load(
    scale = "small",
    type = "land",
    category = "physical",
    destdir = "~/projects/ne_data",
    returnclass = "sf"
  )
  sf::st_agr(world) <- "constant"

  ## transform the map (until now in lat/lon CRS) into the target CRS
  world_transf <- sf::st_transform(world, target_crs)

  ## define boundary coordinates in the target CRS
  zoom_bounds <- define_zoom(lon, lat, 4326)
  zoom_transf <- sf::st_transform(zoom_bounds, target_crs)

  ## crop the map to the boundary coordinates
  world_zoom <- sf::st_crop(world_transf, zoom_transf)

  sf::st_agr(world_zoom) <- "constant"
  world_zoom
}


#' Take a list of all population regions and intersect them with the
#' set of underlying world map
render_ranges <- function(ranges, world, boundary = NULL) {
  rendered <- sf::st_intersection(ranges, sf::st_geometry(world))
  if (!is.null(boundary)) {
    sf::st_agr(rendered) <- "constant"
    rendered <- sf::st_intersection(rendered, sf::st_geometry(boundary))
  }
  ## add a small tag signifying that the ranges have been processed
  ## and intersected over the map of the world
  attr(rendered, "rendered") <- TRUE
  rendered
}


#' Define circular range for a population at a given time
circ_range <- function(pop, time, lon, lat, radius, world) {
  pop_info <- data.frame(pop = pop, time = time)

  # point in the WGS84 geographic CRS
  point_wgs84 <- sf::st_sf(
    pop_info,
    geometry = sf::st_sfc(sf::st_point(c(lon, lat))),
    crs = 4326
  )

  # transform into target CRS
  point <- sf::st_transform(point_wgs84, sf::st_crs(world))
  # expand range into desired radius
  range <- sf::st_buffer(point, radius * 1000)

  # set attributes as constant throughout the geometry
  st_agr(range) <- "constant"

  range
}

#' Define polygon range for a population at a given time
poly_range <- function(pop, time, coords, world, source_crs = "EPSG:4326") {
  pop_info <- data.frame(pop = pop, time = time)
  range <- create_polygon(coords, world, source_crs) %>%
    sf::st_sf(pop_info, geometry = sf::st_geometry(.))
  ## set attributes as constant throughout the geometry
  sf::st_agr(range) <- "constant"
  range
}

#' Expand population radius by a given factor in a given time
expand <- function(region, by, duration, snapshots) {
  check_not_rendered(region)

  start_time <- region$time
  times <- seq(
    start_time,
    start_time - duration,
    length.out = snapshots + 1
  )[-1]

  inter_regions <- list()
  inter_regions[[1]] <- region
  for (i in seq_along(times)) {
    exp_region <- sf::st_buffer(inter_regions[[1]], dist = i * (by / snapshots) * 1000)
    exp_region$time <- times[i]
    inter_regions[[i + 1]] <- exp_region
  }

  inter_regions <- do.call(rbind, inter_regions)
  sf::st_agr(inter_regions) <- "constant"

  inter_regions
}

check_not_rendered <- function(region) {
  if (!is.null(attr(region, "rendered")))
    stop("An already rendered population range object was provided.
Please provide a range object before it was rendered against a map.",
call. = FALSE)
}


#' Move population to a new location in a given amount of time
migrate <- function(region, lon, lat, duration, nslices = 5,
                    source_crs = "EPSG:4326") {
  check_not_rendered(region)

  region_start <- region[nrow(region), ]
  start_time <- region_start$time
  end_lon <- lon
  end_lat <- lat

  target_crs <- sf::st_crs(region)
  
  end_point <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(end_lon, end_lat))),
    crs = source_crs
  ) %>% sf::st_transform(crs = target_crs)

  start_point_geom <- sf::st_geometry(sf::st_centroid(region_start))[[1]]
  end_point_geom <- sf::st_geometry(end_point)[[1]]

  # trajectory of population range shift
  traj <- sf::st_linestring(rbind(start_point_geom, end_point_geom))
  traj_segments <- sf::st_segmentize(traj, sf::st_length(traj) / nslices)

  traj_segments <- sf::st_sf(geometry = sf::st_sfc(traj_segments), crs = target_crs)
  #traj_segments <- sf::st_transform(traj_segments, crs = target_crs)

  traj_points <- sf::st_cast(traj_segments, "POINT")
  traj_points_coords <- sf::st_coordinates(traj_points)
  traj_diffs <- diff(traj_points_coords)
  
  time_slices <- seq(start_time, start_time - duration, length.out = nslices + 1)[-1]
  traj_diffs <- cbind(traj_diffs, time = time_slices)
  
  inter_regions <- list()
  inter_regions[[1]] <- region_start
  for (i in seq_len(nrow(traj_diffs))) {
    shifted_region <- sf::st_geometry(inter_regions[[i]]) + traj_diffs[i, c("X", "Y")]
    inter_regions[[i + 1]] <- sf::st_sf(
      data.frame(pop = region_start$pop, time = traj_diffs[i, "time"]),
      geometry = shifted_region,
      crs = sf::st_crs(inter_regions[[i]])
    )
  }

  inter_regions <- do.call(rbind, inter_regions)
  sf::st_agr(inter_regions) <- "constant"

  inter_regions
}

#' Plot population regions
plot_ranges <- function(..., world, snapshots = F) {
  ranges <- do.call(rbind, list(...))
  
  p_map <-  ggplot() +
    geom_sf(data = world, fill = NA, color = "black") +
    theme_bw()

  if (!is.null(ranges$pop)) {
    if (snapshots)
      p_map <- p_map +
        geom_sf(data = ranges, aes(fill = factor(time)), color = NA) +
        facet_wrap(~ -time)
    else
      p_map <- p_map +
        geom_sf(data = ranges, aes(fill = factor(time)), color = NA, alpha = 0.2)
  }
  if (!is.null(ranges$region)) {
    p_map <- p_map +
      geom_sf(data = ranges, fill = "lightgray", color = NA, alpha = 0.5) +
      geom_sf_label(data = ranges, aes(label = region))
  }

  p_map + coord_sf(crs = sf::st_crs(world)) #, datum = sf::st_crs(world))
}

region_center <- function(region, x = NULL) {
  as.vector(sf::st_coordinates(sf::st_transform(sf::st_centroid(region), source_crs)$geometry[[1]]))
}


