#' Define a population range
#'
#' @param name Name of the population
#' @param time Time of the population appearance
#' @param Ne Effective population size at the time of split
#' @param parent Parent population object or "ancestor" character scalar
#' @param world Object of the type \code{sf} which defines the world
#' @param center Vector of two elements defining a center of a circular range
#' @param radius Scalar defining a radius of a range in kilometers
#' @param coords List of vector pairs, defining corners of the range
#' @param region Geographic region of the class \code{spammr_region}
#' @param remove Time at which the population should be removed
#'
#' @return Object of the \code{spammr_pop} (and \code{sf}) class
#'
#' @export
population <- function(name, parent, Ne, time = NULL, world = NULL,
                       center = NULL, radius = NULL, coords = NULL,
                       region = NULL, remove = NULL) {
  # is this the first population defined in the model?
  if (is.character(parent) && parent == "ancestor") {
    time <- Inf
    if (is.null(world))
      stop("Ancestral population is required to specify its 'world' context",
           call. = FALSE)
  } else if (!is.character(parent) & is.null(time)) {
    stop("The split time of each population (except for the ancestral population) needs to be specified",
         call. = FALSE)
  } else {
    world <- attr(parent[nrow(parent), ], "world")
  }
  # define the population range as a simple geometry object
  # and bind it with the annotation info into an sf object
  if (!is.null(region) & is.null(center)) {
    range <- sf::st_sfc(sf::st_geometry(region))
  } else {
    range <- spatial_range(world, center, radius, coords)
  }
  pop_range <- sf::st_sf(
    data.frame(pop = name, time = time, Ne = Ne, stringsAsFactors = FALSE),
    geometry = range
  )

  sf::st_agr(pop_range) <- "constant"

  # optionally, keep a restricted population region
  if (!is.null(region) & !is.null(center))
    attr(pop_range, "region") <- region

  # when to clean up the population?
  attr(pop_range, "remove") <- ifelse(!is.null(remove), remove, -1)

  # keep a record of the parent population
  if (inherits(parent, "spammr_pop")) {
    attr(pop_range, "parent") <- parent[nrow(parent), ]
    # keep the world as an internal attribute
    attr(pop_range, "world") <- world
  } else if (is.character(parent) & parent == "ancestor") {
    attr(pop_range, "parent") <- "ancestor"
    attr(pop_range, "world") <- world
  } else
    stop("Suspicious parental population specified", call. = FALSE)

  class(pop_range) <- set_class(pop_range, "pop")
  pop_range
}

#' Add a new time snapshot for an already defined population
#'
#' This function allows a more manual control of spatial map changes in addition
#' to the \code{expand} and \code{move} functions
#'
#' @param pop Population object of the \code{spammr} class
#' @param time Time of the current snapshot that is being defined
#' @param Ne Effective population size (stays the same by default)
#' @param center Vector of two elements defining a center of a circular range
#' @param radius Scalar defining a radius of a range in kilometers
#' @param coords List of vector pairs, defining corners of the range
#' @param region Geographic region of the class \code{spammr_region}
#'
#' @return Object of the \code{spammr_pop} (and \code{sf}) class
#'
#' @export
change <- function(pop, time, Ne = NULL,
                   center = NULL, radius = NULL, coords = NULL,
                   region = NULL) {
  if (time %in% pop$time)
    stop("Time point already defined", call. = FALSE)

  if (time > pop[nrow(pop), ]$time)
    warning("Specifying a spatial map at a time point prior to the last spatial map present for the population",
            call. = FALSE)

  if (time < attr(pop, "remove"))
    stop("Cannot update population status after it has been removed")

  world <- attr(pop, "world")
  # define the new population range or re-use the old one
  if (!is.null(region)) {
    range <- sf::st_sfc(sf::st_geometry(region))
  } else if (!is.null(center) & !is.null(radius)){
    range <- spatial_range(world, center, radius, coords)
  } else
    range <- sf::st_geometry(pop[nrow(pop), ])

  if (is.null(Ne)) Ne <- pop[nrow(pop), ]$Ne

  updated <- sf::st_sf(
    data.frame(pop = unique(pop$pop), time = time, Ne = Ne, stringsAsFactors = FALSE),
    geometry = range
  )

  res <- rbind(pop, updated)

  class(res) <- class(pop)
  attr(res, "parent") <- attr(pop, "parent")
  attr(res, "remove") <- attr(pop, "remove")
  attr(res, "world") <- world
  sf::st_agr(res) <- "constant"

  res
}


#' Expand population radius by a given factor in a given time
#'
#' @param pop Spatial population object of \code{spammr_pop} class
#' @param by How many kilometers to expand by?
#' @param start,end When does the spatial population expansion start/end?
#' @param snapshots Number of time slices to split the movement into
#' @param region Geographic region to restrict the expansion to
#'
#' @return Object of the \code{spammr_pop} (and \code{sf}) class
#'
#' @export
expand <- function(pop, by, end, snapshots, start = NULL, region = NULL) {
  check_not_intersected(pop)

  region_start <- pop[nrow(pop), ]

  if (!is.null(start))
    region_start$time <- start

  start_time <- region_start$time
  times <- seq(
    start,
    end,
    length.out = snapshots + 1
  )[-1]

  inter_regions <- list()
  inter_regions[[1]] <- region_start
  for (i in seq_along(times)) {
    exp_region <- sf::st_buffer(inter_regions[[1]], dist = i * (by / snapshots) * 1000)
    exp_region$time <- times[i]
    inter_regions[[i + 1]] <- exp_region
  }

  inter_regions <- rbind(pop, do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the world as an internal attribute
  attr(inter_regions, "world") <- attr(pop, "world")
  # propagate the information about the parental population
  attr(inter_regions, "parent") <- attr(pop, "parent")
  # optionally, add a movement boundary
  attr(inter_regions, "region") <- region
  # retain the cleanup time
  attr(inter_regions, "remove") <- attr(pop, "remove")

  class(inter_regions) <- set_class(inter_regions, "pop")
  inter_regions
}


#' Move population to a new location in a given amount of time
#'
#' @param pop Spatial population object of \code{spammr_pop} class
#' @param trajectory List of two-dimensional vectors [(longitude, latitude)]
#'   specifying the trajectory of the population movement
#' @param start,end Start/end points of the population movement
#' @param snapshots Number of time slices to split the movement into
#'
#' @return Object of the \code{spammr_pop} (and \code{sf}) class
#'
#' @export
move <- function(pop, trajectory, end, snapshots, start = NULL) {
  check_not_intersected(pop)

  # take care of just a single destination point being specified
  if (!is.list(trajectory) & length(trajectory) == 2)
    trajectory <- list(trajectory)

  region_start <- pop[nrow(pop), ]
  if (!is.null(start)) {
    region_start$time <- start
    sf::st_agr(region_start) <- "constant"
  }
  start_time <- region_start$time

  source_crs <- "EPSG:4326"
  target_crs <- sf::st_crs(pop)

  # prepend the coordinates of the first region to the list of "checkpoints"
  # along the way of the movement
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

  time_slices <- seq(start_time, end, length.out = nrow(traj_points))[-1]
  traj_diffs <- cbind(traj_diffs, time = time_slices)

  inter_regions <- list()
  inter_regions[[1]] <- region_start
  for (i in seq_len(nrow(traj_diffs))) {
    shifted_region <- sf::st_geometry(inter_regions[[i]]) + traj_diffs[i, c("X", "Y")]
    inter_regions[[i + 1]] <- sf::st_sf(
      data.frame(
        pop = region_start$pop,
        time = traj_diffs[i, "time"],
        Ne = region_start$Ne,
        stringsAsFactors = FALSE
      ),
      geometry = shifted_region,
      crs = sf::st_crs(inter_regions[[i]])
    )
  }

  inter_regions <- rbind(pop, do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the world as an internal attribute
  attr(inter_regions, "world") <- attr(pop, "world")
  # propagate the information about the parental population
  attr(inter_regions, "parent") <- attr(pop, "parent")
  # retain the cleanup time
  attr(inter_regions, "remove") <- attr(pop, "remove")

  class(inter_regions) <- set_class(inter_regions, "pop")

  inter_regions
}


#' Define a geographic region
#'
#' @param name Name of the geographic region
#' @param world Object of the type \code{sf} which defines the world
#' @param coords List of vector pairs, defining corners of the range
#'
#' @return Object of the \code{spammr_region} (and \code{sf}) class
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
#' @param xrange Numeric vector with minimum and maximum longitude
#' @param yrange Numeric vector with minimum and maximum latitude
#' @param crs Coordinate Reference System to use for all spatial
#'   operations (default is WGS-84 or EPSG:4326 CRS)
#'
#' @return Object of the \code{spammr_world} (and \code{sf}) class
#'
#' @export
map <- function(xrange, yrange, crs = "EPSG:4326") {
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
  zoom_bounds <- define_zoom(xrange, yrange, "EPSG:4326")
  zoom_transf <- sf::st_transform(zoom_bounds, crs)

  ## crop the map to the boundary coordinates
  world_zoom <- sf::st_crop(world_transf, zoom_transf)

  sf::st_agr(world_zoom) <- "constant"

  class(world_zoom) <- set_class(world_zoom, "world")

  world_zoom
}


#' Define an admixture event
#'
#' @param from,to Population range objects of the class \code{spammr_pop}
#' @param rate Scalar value in the range (0, 1] specifying the proportion of
#'   migration over given time period
#' @param start,end Start and end of the admixture event
#' @param overlap Proportion of the spatial range of the migrating population
#'   \code{from} required to overlap with the range of the \code{to} population.
#'   Value \code{FALSE} disables the overlap requirement, allowing admixture
#'   between non-overlapping populations.
#'
#' @return Object of the class data.frame
#'
#' @export
admixture <- function(from, to, rate, start, end, overlap = 0.2) {
  from_name <- unique(from$pop)
  to_name <- unique(to$pop)

  # get the last specified spatial maps before the admixture time
  region_from <- intersect_features(from[from$time >= start, ] %>% .[nrow(.), ])
  region_to <- intersect_features(to[to$time >= start, ] %>% .[nrow(.), ])

  if (nrow(region_from) == 0)
    stop(sprintf("No spatial map defined for %s at/before the time %d",
                 from_name, start),
         call. = FALSE)
  if (nrow(region_to) == 0)
    stop(sprintf("No spatial map defined for %s at/before the time %d",
                 to_name, start),
         call. = FALSE)

  # make sure the population is not removed during the the admixture period
  from_remove <- attr(from, "remove")
  to_remove <- attr(to, "remove")
  if (from_remove > start | from_remove > end) {
    stop(sprintf(
      "Population %s scheduled for removal at time %d,
which is outside of the %d-%d admixture time range",
      from_name, from_remove, start, end),
      call. = FALSE
    )
  }
  if (to_remove > start | to_remove > end) {
    stop(sprintf("Population %s scheduled for removal at time %d which is
outside of the specified %d-%d admixture time window.",
      to_name, to_remove, start, end),
      call. = FALSE
    )
  }

  # calculate the overlap of spatial ranges between source and target
  region_overlap <- sf::st_intersection(region_from, region_to)
  area_overlap <- as.numeric(sum(sf::st_area(region_overlap)))
  area_from <- as.numeric(sum(sf::st_area(region_from)))

  if (overlap != FALSE & (length(area_overlap) == 0 | area_overlap / area_from < overlap)) {
    stop(sprintf("
Not a sufficient overlap between population ranges of %s and %s
at time %d. The required overlap is %.2f but the current overlap is
%f.

Please check the spatial maps of both populations by running
`plot(%s, %s, pop_facets = F)` and either adjust the admixture
parameters or add `overlap = F` which will instruct spammr to simulate
admixture without spatial overlap.",
      from_name, to_name, start, overlap, area_overlap / area_from,
      deparse(substitute(from)),
      deparse(substitute(to)), call. = FALSE))
  }

  data.frame(
    from_name = from_name,
    to_name = to_name,
    tstart = start,
    tend = end,
    rate = rate,
    overlap = as.integer(overlap == TRUE | overlap > 0),
    stringsAsFactors = FALSE
  )
}
