#' Define a population range
#'
#' @param name Name of the population
#' @param time Time of the population appearance
#' @param Ne Effective population size at the time of split
#' @param parent Parent population object or "ancestor" character scalar
#' @param world Object of the type 'sf' which defines the world
#' @param center Vector of two elements defining a center of a circular range
#' @param radius Scalar defining a radius of a range in kilometers
#' @param coords List of vector pairs, defining corners of the range
#'
#' @export
population <- function(name, time, Ne, parent,
                       world, center = NULL, radius = NULL, coords = NULL,
                       region = NULL) {
  # define the population range as a simple geometry object
  # and bind it with the annotation info into an sf object
  if (!is.null(region) & is.null(center)) {
    range <- sf::st_sfc(sf::st_geometry(region))
  } else {
    range <- spatial_range(world, center, radius, coords)
  }
  pop_range <- sf::st_sf(
    data.frame(pop = name, time = time, Ne = Ne),
    geometry = range
  )

  sf::st_agr(pop_range) <- "constant"

  # keep the world as an internal attribute
  attr(pop_range, "world") <- world
  # optionally, keep a restricted population region
  if (!is.null(region) & !is.null(center))
    attr(pop_range, "region") <- region

  # keep a record of the parent population
  if (inherits(parent, "spammr_pop"))
    attr(pop_range, "parent") <- parent[nrow(parent), ]
  else if (is.character(parent) & parent == "ancestor")
    attr(pop_range, "parent") <- "ancestor"
  else
    stop("Suspicious parental population specified", call. = FALSE)

  class(pop_range) <- set_class(pop_range, "pop")
  pop_range
}


#' Expand population radius by a given factor in a given time
#'
#' @param pop Spatial population object of 'spammr_pop' class
#' @param by How many kilometers to expand by?
#' @param duration Duration of the spatial population expansion
#' @param snapshots Number of time slices to split the movement into
#' @param region Geographic region to restrict the expansion to
#'
#' @export
expand <- function(pop, by, duration, snapshots, region = NULL) {
  check_not_intersected(pop)

  region_start <- pop[nrow(pop), ]

  start_time <- region_start$time
  times <- seq(
    start_time,
    start_time - duration,
    length.out = snapshots + 1
  )[-1]

  inter_regions <- list()
  inter_regions[[1]] <- region_start
  for (i in seq_along(times)) {
    exp_region <- sf::st_buffer(inter_regions[[1]], dist = i * (by / snapshots) * 1000)
    exp_region$time <- times[i]
    inter_regions[[i + 1]] <- exp_region
  }

  inter_regions <- rbind(pop[-nrow(pop), ], do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the world as an internal attribute
  attr(inter_regions, "world") <- attr(pop, "world")
  # propagate the information about the parental population
  attr(inter_regions, "parent") <- attr(pop, "parent")
  # optionally, add a migration boundary
  attr(inter_regions, "region") <- region

  class(inter_regions) <- set_class(inter_regions, "pop")
  inter_regions
}


#' Move population to a new location in a given amount of time
#'
#' @param pop Spatial population object of 'spammr_pop' class
#' @param trajectory List of two-dimensional vectors [(longitude, latitude)]
#'   specifying the trajectory of the population movement
#' @param duration Duration of the population movement
#' @param snapshots Number of time slices to split the movement into
#'
#' @export
migrate <- function(pop, trajectory, duration, snapshots) {
  check_not_intersected(pop)

  # take care of just a single destination point being specified
  if (!is.list(trajectory) & length(trajectory) == 2)
    trajectory <- list(trajectory)

  region_start <- pop[nrow(pop), ]
  start_time <- region_start$time

  source_crs <- "EPSG:4326"
  target_crs <- sf::st_crs(pop)

  # prepend the coordinates of the first region to the list of "checkpoints"
  # along the way of the migration
  start_coords <- sf::st_centroid(region_start) %>%
    sf::st_transform(crs = source_crs) %>%
    sf::st_coordinates()
  checkpoints <- c(list(as.vector(start_coords)), trajectory)

  traj <- sf::st_linestring(do.call(rbind, checkpoints)) %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = source_crs) %>%
    sf::st_transform(crs = target_crs)

  traj_segments <- sf::st_segmentize(traj, sf::st_length(traj) / snapshots)

  traj_points <- sf::st_cast(traj_segments, "POINT")
  traj_points_coords <- sf::st_coordinates(traj_points)
  traj_diffs <- diff(traj_points_coords)

  time_slices <- seq(start_time, start_time - duration, length.out = nrow(traj_points))[-1]
  traj_diffs <- cbind(traj_diffs, time = time_slices)

  inter_regions <- list()
  inter_regions[[1]] <- region_start
  for (i in seq_len(nrow(traj_diffs))) {
    shifted_region <- sf::st_geometry(inter_regions[[i]]) + traj_diffs[i, c("X", "Y")]
    inter_regions[[i + 1]] <- sf::st_sf(
      data.frame(
        pop = region_start$pop,
        time = traj_diffs[i, "time"],
        Ne = region_start$Ne
      ),
      geometry = shifted_region,
      crs = sf::st_crs(inter_regions[[i]])
    )
  }

  inter_regions <- rbind(pop[-nrow(pop), ], do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the world as an internal attribute
  attr(inter_regions, "world") <- attr(pop, "world")
  # propagate the information about the parental population
  attr(inter_regions, "parent") <- attr(pop, "parent")

  class(inter_regions) <- set_class(inter_regions, "pop")

  inter_regions
}


#' Define a geographic region
#'
#' @param name Name of the geographic region
#' @param world Object of the type 'sf' which defines the world
#' @param coords List of vector pairs, defining corners of the range
#'
#' @export
region <- function(name, world, coords) {
  region <- sf::st_sf(
    region = name,
    geometry = spatial_range(world, coords = coords)
  )
  sf::st_agr(region) <- "constant"

  # keep the world as an internal attribute
  attr(region, "world") <- world

  class(region) <- set_class(region, "region")
  region
}

#' Define a world map for all spatial operations
#'
#' Download Natural Earth land area map data, zoom on a given window
#' determined by longitude and latitude coordinates and transform to a
#' specified projection
#'
#' @param lon_range Numeric vector with minimum and maximum longitude
#' @param lat_range Numeric vector with minimum and maximum latitude
#' @param crs Coordinate Reference System to use for all spatial
#'   operations (default is WGS-84 or EPSG:4326 CRS)
#'
#' @export
world_map <- function(lon_range, lat_range, crs = "EPSG:4326") {
  ## load the map data (either from a cache location on disk or from
  ## the server)
  ## world <- rnaturalearth::ne_load(
  ##   scale = "small",
  ##   type = "land",
  ##   category = "physical",
  ##   destdir = "~/projects/ne_data",
  ##   returnclass = "sf"
  ## )
  world <- rnaturalearth::ne_download(
    scale = "small",
    type = "land",
    category = "physical",
    returnclass = "sf"
  )
  sf::st_agr(world) <- "constant"

  ## transform the map (default geographic CRS) into the target CRS
  world_transf <- sf::st_transform(world, crs)

  ## define boundary coordinates in the target CRS
  zoom_bounds <- define_zoom(lon_range, lat_range, "EPSG:4326")
  zoom_transf <- sf::st_transform(zoom_bounds, crs)

  ## crop the map to the boundary coordinates
  world_zoom <- sf::st_crop(world_transf, zoom_transf)

  sf::st_agr(world_zoom) <- "constant"

  class(world_zoom) <- set_class(world_zoom, "world")

  world_zoom
}
