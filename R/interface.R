#' Define a population
#'
#' Defines the parameters of a population (non-spatial and spatial).
#'
#' There are four ways to specify a spatial boundary: i) circular range
#' specified using a center coordinate and a radisu, ii) polygon specified as a
#' list of two-dimensional vector coordinates, iii) polygon as in ii), but
#' defined (and named) using the \code{region} function, iv) with just a world
#' map specified (circular or polygon range parameters set to the default
#' \code{NULL} value), the population will be allowed to occupy the entire
#' landscape.
#'
#' @param name Name of the population
#' @param time Time of the population's first appearance
#' @param N Number of individuals at the time of first appearance
#' @param parent Parent population object or "ancestor" (indicating that the
#'   population does not have an ancestor, and that it is the first population
#'   in its "lineage")
#' @param map Object of the type \code{slendr_map} which defines the world
#'   context (created using the \code{world} function). If the value
#'   \code{FALSE} is provided, a non-spatial model will be run.
#' @param center Two-dimensional vector specifying the center of the circular
#'   range
#' @param radius Radius of the circular range
#' @param polygon List of vector pairs, defining corners of the polygon range or
#'   a geographic region of the class \code{slendr_region} from which the
#'   polygon coordinates will be extracted (see the \code{region() function})
#' @param remove Time at which the population should be removed
#' @param intersect Intersect the population's boundaries with landscape
#'   features?
#' @param competition_dist,mate_dist Maximum spatial competition and mating
#'   choice distance
#' @param dispersal_dist Standard deviation of the normal distribution of the
#'   distance that offspring disperses from its parent
#' @param dispersal_fun Distribution function governing the dispersal of
#'   offspring. One of "normal", "uniform", "cauchy", or "exponential".
#' @param aquatic Is the species aquatic (\code{FALSE} by default, i.e.
#'   terrestrial species)?
#'
#' @return Object of the class \code{slendr_pop}
#'
#' @export
#'
#' @example man/examples/model_definition.R
population <- function(name, time, N, parent = "ancestor", map = FALSE,
                       center = NULL, radius = NULL, polygon = NULL,
                       remove = NULL, intersect = TRUE,
                       competition_dist = NA, mate_dist = NA, dispersal_dist = NA,
                       dispersal_fun = NULL, aquatic = FALSE) {
  # is this the first population defined in the model?
  if (is.character(parent) && parent == "ancestor") {
    if (!is.logical(map) && !inherits(map, "slendr_map"))
      stop("Ancestral population must specify its 'map'", call. = FALSE)
    else
      map <- map
  } else {
    check_split_time(time, parent)
    map <- attr(parent, "map")
  }
  time <- as.integer(time)
  N <- as.integer(N)

  if (inherits(map, "slendr_map")) {
    # define the population range as a simple geometry object
    # and bind it with the annotation info into an sf object
    if (is.null(polygon) && is.null(center) && is.null(radius))
      geometry <- sf::st_bbox(map) %>% sf::st_as_sfc()
    else if (!is.null(polygon) & inherits(polygon, "slendr_region"))
      geometry <- sf::st_geometry(polygon)
    else
      geometry <- define_boundary(map, center, radius, polygon)

    pop <- sf::st_sf(
      data.frame(pop = name, tmap = time, stringsAsFactors = FALSE),
      geometry = geometry
    )
    sf::st_agr(pop) <- "constant"
    attr(pop, "intersect") <- intersect
    attr(pop, "aquatic") <- aquatic
  } else
    pop <- list(pop = name, time = time)

  # when to clean up the population?
  attr(pop, "remove") <- if (!is.null(remove)) remove else -1

  # keep a record of the parent population
  if (inherits(parent, "slendr_pop"))
    attr(pop, "parent") <- parent
  else if (is.character(parent) & parent == "ancestor")
    attr(pop, "parent") <- "ancestor"
  else
    stop("Suspicious parent population", call. = FALSE)

  attr(pop, "map") <- map

  dispersal_fun <- kernel_fun(dispersal_fun)

  # create the first population history event - population split
  attr(pop, "history") <- list(data.frame(
    pop =  name,
    event = "split",
    time = time,
    N = N,
    competition_dist = competition_dist,
    mate_dist = mate_dist,
    dispersal_dist = dispersal_dist,
    dispersal_fun = dispersal_fun
  ))

  class(pop) <- set_class(pop, "pop")

  pop
}


#' Move the population to a new location in a given amount of time
#'
#' This function defines a displacement of a population along a given trajectory
#' in a given time frame
#'
#' @param pop Object of the class \code{slendr_pop}
#' @param trajectory List of two-dimensional vectors (longitude, latitude)
#'   specifying the migration trajectory
#' @param start,end Start/end points of the population migration
#' @param overlap Minimum overlap between subsequent spatial boundaries
#' @param snapshots The number of intermediate snapshots (overrides the
#'   \code{overlap} parameter)
#' @param verbose Show the progress of searching through the number of
#'   sufficient snapshots?
#'
#' @return Object of the class \code{slendr_pop}
#'
#' @export
#'
#' @example man/examples/model_definition.R
move <- function(pop, trajectory, end, start, overlap = 0.8, snapshots = NULL,
                 verbose = TRUE) {
  if (!has_map(pop)) stop("This operation is only allowed for spatial models", call. = FALSE)

  check_event_time(c(start, end), pop)
  check_removal_time(start, pop)
  check_removal_time(end, pop)

  if (!is.null(snapshots))
    if (snapshots <= 0)
      stop("The number of snapshots must be a non-negative integer", call. = FALSE)

  if (!(overlap > 0 & overlap < 1))
    stop("The required overlap between subsequent spatial maps must be a number between 0 and 1",
         call. = FALSE)

  map <- attr(pop, "map")

  # take care of just a single destination point being specified
  if (!is.list(trajectory) & length(trajectory) == 2)
    trajectory <- list(trajectory)

  # get the last available population boundary
  region_start <- pop[nrow(pop), ]
  region_start$tmap <- start
  sf::st_agr(region_start) <- "constant"

  # prepend the coordinates of the first region to the list of "checkpoints"
  # along the way of the movement
  start_coords <- sf::st_centroid(region_start)

  if (has_crs(map)) {
    source_crs <- "EPSG:4326"
    target_crs <- sf::st_crs(pop)
    start_coords <- sf::st_transform(start_coords, crs = source_crs)
  }

  start_coords <- sf::st_coordinates(start_coords)
  checkpoints <- c(list(as.vector(start_coords)), trajectory)

  traj <- sf::st_linestring(do.call(rbind, checkpoints)) %>% sf::st_sfc()

  if (has_crs(map)) {
    traj <- sf::st_sf(traj, crs = source_crs) %>%
      sf::st_transform(crs = target_crs)
  } else {
    traj <- sf::st_sf(traj)
  }

  if (is.null(snapshots)) {
    n <- 1
    message("Iterative search for the minimum sufficient number of intermediate ",
            "spatial snapshots, starting at ", n, ". This should only take a couple of ",
            "seconds, but if you don't want to wait, you can set `snapshots = N` manually.")
  } else
    n <- snapshots

  # iterate through the number of intermediate spatial boundaries to reach
  # the required overlap between subsequent spatial maps
  repeat {
    traj_segments <- sf::st_segmentize(traj, sf::st_length(traj) / n)

    traj_points <- sf::st_cast(traj_segments, "POINT")
    traj_points_coords <- sf::st_coordinates(traj_points)
    traj_diffs <- diff(traj_points_coords)

    time_slices <- seq(start, end, length.out = nrow(traj_points))[-1]
    traj_diffs <- cbind(traj_diffs, time = time_slices)

    inter_regions <- list()
    inter_regions[[1]] <- region_start
    for (i in seq_len(nrow(traj_diffs))) {
      shifted_region <- sf::st_geometry(inter_regions[[i]]) + traj_diffs[i, c("X", "Y")]
      inter_regions[[i + 1]] <- sf::st_sf(
        data.frame(
          pop = region_start$pop,
          tmap = traj_diffs[i, "time"],
          stringsAsFactors = FALSE
        ),
        geometry = shifted_region,
        crs = sf::st_crs(inter_regions[[i]])
      )
      sf::st_agr(inter_regions[[i + 1]]) <- "constant"
    }

    if (!is.null(snapshots)) break

    # calculate the overlap between subsequent spatial snapshots
    overlaps <- compute_overlaps(do.call(rbind, inter_regions))

    if (all(overlaps >= overlap)) {
      message("The required ", sprintf("%.1f%%", 100 * overlap),
              " overlap between subsequent spatial maps has been met")
      break
    } else {
      n <- n + 1
      if (verbose)
        message("- testing ", n, " snapshots")
    }
  }

  inter_regions <- rbind(pop, do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  result <- copy_attributes(
    inter_regions, pop,
    c("map", "parent", "remove", "intersect", "aquatic", "history")
  )

  attr(result, "history") <- append(attr(result, "history"), list(data.frame(
    pop =  unique(region_start$pop),
    event = "move",
    tstart = start,
    tend = end
  )))

  result
}


#' Expand the population range
#'
#' Expands the spatial population range by a specified distance in a given
#' time-window
#'
#' @param pop Object of the class \code{slendr_pop}
#' @param by How many units of distance to expand by?
#' @param start,end When does the expansion start/end?
#' @param overlap Minimum overlap between subsequent spatial boundaries
#' @param snapshots The number of intermediate snapshots (overrides the
#'   \code{overlap} parameter)
#' @param polygon Geographic region to restrict the expansion to
#' @param lock Maintain the same density of individuals. If
#'   \code{FALSE} (the default), the number of individuals in the
#'   population will not change. If \code{TRUE}, the number of
#'   individuals simulated will be changed (increased or decreased)
#'   appropriately, to match the new population range area.
#' @param verbose Report on the progress of generating intermediate spatial
#'   boundaries?
#'
#' @return Object of the class \code{slendr_pop}
#'
#' @export
#'
#' @example man/examples/model_definition.R
expand <- function(pop, by, end, start, overlap = 0.8, snapshots = NULL,
                   polygon = NULL, lock = FALSE, verbose = TRUE) {
  if (!has_map(pop)) stop("This operation is only allowed for spatial models", call. = FALSE)
  shrink_or_expand(pop, by, end, start, overlap, snapshots, polygon, lock, verbose)
}


#' Shrink the population range
#'
#' Shrinks the spatial population range by a specified distance in a given
#' time-window
#'
#' @param pop Object of the class \code{slendr_pop}
#' @param by How many units of distance to shrink by?
#' @param start,end When does the boundary shrinking start/end?
#' @param overlap Minimum overlap between subsequent spatial boundaries
#' @param snapshots The number of intermediate snapshots (overrides the
#'   \code{overlap} parameter)
#' @param lock Maintain the same density of individuals. If
#'   \code{FALSE} (the default), the number of individuals in the
#'   population will not change. If \code{TRUE}, the number of
#'   individuals simulated will be changed (increased or decreased)
#'   appropriately, to match the new population range area.
#' @param verbose Report on the progress of generating intermediate spatial
#'   boundaries?
#'
#' @return Object of the class \code{slendr_pop}
#'
#' @export
#'
#' @example man/examples/model_definition.R
shrink <- function(pop, by, end, start, overlap = 0.8, snapshots = NULL,
                   lock = FALSE, verbose = TRUE) {
  shrink_or_expand(pop, -by, end, start, overlap, snapshots, polygon = NULL, lock, verbose)
}


#' Update the population range
#'
#' This function allows a more manual control of spatial map changes
#' in addition to the \code{expand} and \code{move} functions
#'
#' @param pop Object of the class \code{slendr_pop}
#' @param time Time of the change
#' @param center Two-dimensional vector specifying the center of the
#'   circular range
#' @param radius Radius of the circular range
#' @param polygon List of vector pairs, defining corners of the
#'   polygon range (see also the \code{region} argument) or a
#'   geographic region of the class \code{slendr_region} from which
#'   the polygon coordinates will be extracted
#' @param lock Maintain the same density of individuals. If
#'   \code{FALSE} (the default), the number of individuals in the
#'   population will not change. If \code{TRUE}, the number of
#'   individuals simulated will be changed (increased or decreased)
#'   appropriately, to match the new population range area.
#'
#' @return Object of the class \code{slendr_pop}
#'
#' @export
#'
#' @example man/examples/model_definition.R
boundary <- function(pop, time, center = NULL, radius = NULL,
                     polygon = NULL, lock = FALSE) {
  if (!has_map(pop))
    stop("This operation is only allowed for spatial models", call. = FALSE)

  check_event_time(time, pop)
  check_removal_time(time, pop)

  map <- attr(pop, "map")

  # define the population range as a simple geometry object
  # and bind it with the annotation info into an sf object
  if (!is.null(polygon) & inherits(polygon, "slendr_region"))
    geometry <- sf::st_geometry(polygon)
  else
    geometry <- define_boundary(map, center, radius, polygon)

  updated <- sf::st_sf(
    data.frame(pop = unique(pop$pop), tmap = time, stringsAsFactors = FALSE),
    geometry = geometry
  )

  # Let's not enforce this given that the point of this function is to allow
  # full manual control over the boundary dynamics:
  # if (compute_overlaps(rbind(updated, pop[nrow(pop), ])) < overlap)
  #   stop("Insufficient overlap with the last active spatial boundary (",
  #        "please adjust the new spatial boundary or adjust the `overlap = `",
  #        "parameter for less stringent checking)", call. = FALSE)

  combined <- rbind(pop, updated)
  sf::st_agr(combined) <- "constant"

  result <- copy_attributes(
    combined, pop,
    c("map", "parent", "remove", "intersect", "aquatic", "history")
  )

  attr(result, "history") <- append(attr(result, "history"), list(data.frame(
    event = "range",
    time = time
  )))

  if (lock) {
    areas <- slendr::area(result)$area
    area_change <- areas[length(areas)] / areas[length(areas) - 1]
    prev_N <- utils::tail(sapply(attributes(pop)$history, function(event) event$N), 1)
    new_N <- round(area_change * prev_N)
    result <- resize(result, N = new_N, time = time, how = "step")
  }

  result
}


#' Change the population size
#'
#' Resizes the population starting from the current value of \code{N}
#' individuals to the specified value
#'
#' In the case of exponential size change, if the final \code{N} is larger than
#' the current size, the population will be exponentially growing over the
#' specified time period until it reaches \code{N} individuals. If \code{N} is
#' smaller, the population will shrink exponentially.
#'
#' @param pop Object of the class \code{slendr_pop}
#' @param N Population size after the change
#' @param how How to change the population size (options are \code{"step"} or
#'   \code{"exponential"})
#' @param time Time of the population size change
#' @param end End of the population size change period (used for exponential
#'   change events)
#'
#' @export
#'
#' @example man/examples/model_definition.R
resize <- function(pop, N, how, time, end = NULL) {
  if (N < 1) stop("resize(): Only positive, non-zero population sizes are allowed", call. = FALSE)

  if (!how %in% c("step", "exponential"))
    stop("resize(): Only 'step' or 'exponential' are allowed as arguments for the 'how' parameter", call. = FALSE)

  if (how == "exponential" & is.null(end))
    stop("resize(): Start-end period of the exponential growth must be specified", call. = FALSE)

  # get the last active population size
  prev_N <- sapply(attr(pop, "history"), function(event) event$N) %>%
    Filter(Negate(is.null), .) %>%
    unlist %>%
    utils::tail(1)

  change <- data.frame(
    pop =  unique(pop$pop),
    event = "resize",
    how = how,
    N = N,
    prev_N = prev_N,
    tresize = time
  )

  if (how == "step") {
    check_event_time(time, pop)
    check_removal_time(time, pop)
    change$tend <- NA
  } else {
    check_event_time(c(time, end), pop)
    check_removal_time(time, pop)
    check_removal_time(end, pop)
    change$tend <- end
  }

  attr(pop, "history") <- append(attr(pop, "history"), list(change))

  pop
}


#' Change dispersal parameters
#'
#' Changes either the competition interactive distance, mating choice distance,
#' or the dispersal of offspring from its parent
#'
#' @param pop Object of the class \code{slendr_pop}
#' @param time Time of the population size change
#' @param competition_dist,mate_dist Maximum spatial competition and mating
#'   choice distance
#' @param dispersal_dist Standard deviation of the normal distribution of the
#'   distance that offspring disperses from its parent
#' @param dispersal_fun Distribution function governing the dispersal of
#'   offspring. One of "normal", "uniform", "cauchy", or "exponential".
#'
#' @export
#'
#' @example man/examples/model_definition.R
dispersal <- function(pop, time, competition_dist = NA, mate_dist = NA, dispersal_dist = NA,
                      dispersal_fun = NULL) {
  if (!has_map(pop)) stop("This operation is only allowed for spatial models", call. = FALSE)

  if (is.na(competition_dist) && is.na(mate_dist) && is.na(dispersal_dist) &&
      is.null(dispersal_fun))
    stop("At least one spatial interaction parameter must be specified", call. = FALSE)

  if (any(c(competition_dist, mate_dist, dispersal_dist) < 0, na.rm = TRUE))
    stop("Spatial interaction parameters can only have positive, non-zero values", call. = FALSE)

  dispersal_fun <- kernel_fun(dispersal_fun)

  map <- attr(pop, "map")
  if (!is.na(competition_dist)) check_resolution(map, competition_dist)
  if (!is.na(mate_dist)) check_resolution(map, mate_dist)
  if (!is.na(dispersal_dist)) check_resolution(map, dispersal_dist)

  check_event_time(time, pop)
  check_removal_time(time, pop)

  change <- data.frame(
    pop =  unique(pop$pop),
    event = "dispersal",
    time = time,
    competition_dist = competition_dist,
    mate_dist = mate_dist,
    dispersal_dist = dispersal_dist,
    dispersal_fun = dispersal_fun
  )

  attr(pop, "history") <- append(attr(pop, "history"), list(change))

  pop
}


#' Define a gene flow event between two populations
#'
#' @param from,to Objects of the class \code{slendr_pop}
#' @param rate Scalar value in the range (0, 1] specifying the
#'   proportion of migration over given time period
#' @param start,end Start and end of the geneflow event
#' @param overlap Require spatial overlap between admixing
#'   populations?  (default \code{TRUE})
#'
#' @return Object of the class data.frame
#'
#' @export
#'
#' @example man/examples/model_definition.R
geneflow <- function(from, to, rate, start, end, overlap = TRUE) {
  if ((has_map(from) && !has_map(to)) || (!has_map(from) && has_map(to)))
    stop("Both or neither populations must be spatial", call. = FALSE)

  # make sure the population is not removed during the the admixture period
  check_removal_time(start, from)
  check_removal_time(end, from)
  check_removal_time(start, to)
  check_removal_time(end, to)

  from_name <- unique(from$pop)
  to_name <- unique(to$pop)

  if (has_map(from) && has_map(to)) {
    if (from$tmap[1] <= start & from$tmap[1] <= end &
        to$tmap[1] <= start & to$tmap[1] <= end)
      comp <- `<=`
    else if (from$tmap[1] >= start & from$tmap[1] >= end &
             to$tmap[1] >= start & to$tmap[1] >= end)
      comp <- `>=`
    else
      stop(sprintf("Specified times are not consistent with the assumed direction of
time (geneflow %s -> %s in the time window %s-%s)",
                   from_name, to_name, start, end), call. = FALSE)

    # get the last specified spatial maps before the geneflow time
    region_from <- intersect_features(from[comp(from$tmap, start), ] %>% .[nrow(.), ])
    region_to <- intersect_features(to[comp(to$tmap, start), ] %>% .[nrow(.), ])

    if (nrow(region_from) == 0)
      stop(sprintf("No spatial map defined for %s at/before the time %d",
                   from_name, start),
           call. = FALSE)
    if (nrow(region_to) == 0)
      stop(sprintf("No spatial map defined for %s at/before the time %d",
                   to_name, start),
           call. = FALSE)

    # calculate the overlap of spatial ranges between source and target
    region_overlap <- sf::st_intersection(region_from, region_to)
    area_overlap <- as.numeric(sum(sf::st_area(region_overlap)))

    if (overlap && area_overlap == 0) {
      stop(sprintf("No overlap between population ranges of %s and %s at time %d.

  Please check the spatial maps of both populations by running
  `plot(%s, %s)` and adjust them accordingly. Alternatively, in case
  this makes sense for your model, you can add `overlap = F` which
  will instruct slendr to simulate geneflow without spatial overlap
  between populations.",
  from_name, to_name, start, deparse(substitute(from)),
  deparse(substitute(to))), call. = FALSE)
    }
  } else
    overlap <- FALSE

  data.frame(
    from_name = from_name,
    to_name = to_name,
    tstart = start,
    tend = end,
    rate = rate,
    overlap = overlap,
    stringsAsFactors = FALSE
  )
}


#' Define a world map for all spatial operations
#'
#' Defines either an abstract geographic landscape (blank or
#' containing user-defined landscape) or using a real Earth
#' cartographic data from the Natural Earth project
#' (<https://www.naturalearthdata.com>).
#'
#' @param xrange Two-dimensional vector specifying minimum and maximum
#'   horizontal range ("longitude" if using real Earth cartographic
#'   data)
#' @param yrange Two-dimensional vector specifying minimum and maximum
#'   vertical range ("latitude" if using real Earth cartographic data)
#' @param landscape Either "blank" (for blank abstract geography),
#'   "naturalearth" (for real Earth geography) or an object of the
#'   class \code{sf} defining abstract geographic features of the
#'   world
#' @param crs EPSG code of a coordinate reference system to use for
#'   spatial operations. No CRS is assumed by default (\code{NULL}),
#'   implying an abstract landscape not tied to any real-world
#'   geographic region (when \code{landscape = "blank"} or when
#'   \code{landscape} is a custom-defined geographic landscape), or
#'   implying WGS-84 (EPSG 4326) coordinate system when a real Earth
#'   landscape was defined (\code{landscape = "naturalearth"}).
#' @param ne_dir Path to the directory where Natural Earth data was
#'   manually downloaded and unzipped from
#'   <https://www.naturalearthdata.com/downloads/110m-physical-vectors/>
#'   (used only when \code{landscape = "naturalearth"})
#'
#' @return Object of the class \code{slendr_map}
#'
#' @export
#'
#' @example man/examples/spatial_functions.R
world <- function(xrange, yrange, landscape = "naturalearth", crs = NULL, ne_dir = NULL) {
  if (inherits(landscape, "sf")) { # a landscape defined by the user
    cropped_landscape <- sf::st_crop(
      landscape,
      xmin = xrange[1], xmax = xrange[2],
      ymin = yrange[1], ymax = yrange[2]
    )
    map <- sf::st_sf(landscape = sf::st_geometry(cropped_landscape)) %>%
      set_bbox(xmin = xrange[1], xmax = xrange[2], ymin = yrange[1], ymax = yrange[2])
  } else if (landscape == "blank") { # an empty abstract landscape
    map <- sf::st_sf(geometry = sf::st_sfc()) %>%
      set_bbox(xmin = xrange[1], xmax = xrange[2], ymin = yrange[1], ymax = yrange[2])
  } else if (landscape == "naturalearth") {  # Natural Earth data vector landscape
    if (is.null(ne_dir)) {
      ne_dir <- tempdir()
      ne_file <- file.path(ne_dir, "ne_110m_land.zip")
      utils::download.file(
        url = "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_land.zip",
        destfile = ne_file, quiet = TRUE
      )
      utils::unzip(ne_file, exdir = ne_dir)
    }
    map_raw <- rnaturalearth::ne_load(
      scale = "small", type = "land", category = "physical",
      returnclass = "sf", destdir = ne_dir
    )
    sf::st_agr(map_raw) <- "constant"

    ## transform the map (default geographic CRS) into the target CRS
    map_transf <- sf::st_transform(map_raw, crs) %>% sf::st_make_valid()

    ## define boundary coordinates in the target CRS
    zoom_bounds <- define_zoom(xrange, yrange, "EPSG:4326")
    zoom_transf <- sf::st_transform(zoom_bounds, crs) %>% sf::st_make_valid()

    ## crop the map to the boundary coordinates
    map <- sf::st_crop(map_transf, zoom_transf)
  } else {
    stop("Landscape has to be either 'blank', 'naturalearth' or an object of the class 'sf'",
         call. = FALSE)
  }

  sf::st_agr(map) <- "constant"

  class(map) <- set_class(map, "map")
  attr(map, "xrange") <- xrange
  attr(map, "yrange") <- yrange

  map
}


#' Define a geographic region
#'
#' Creates a geographic region (a polygon) on a given map and gives it
#' a name. This can be used to define objects which can be reused in
#' multiple places in a slendr script (such as \code{region} arguments
#' of \code{population}) without having to repeatedly define polygon
#' coordinates.
#'
#' @param name Name of the geographic region
#' @param map Object of the type \code{sf} which defines the map
#' @param center Two-dimensional vector specifying the center of the
#'   circular range
#' @param radius Radius of the circular range
#' @param polygon List of vector pairs, defining corners of the
#'   polygon range or a geographic region of the class
#'   \code{slendr_region} from which the polygon coordinates will be
#'   extracted (see the \code{region() function})
#'
#' @return Object of the class \code{slendr_region}
#'
#' @export
#'
#' @example man/examples/spatial_functions.R
region <- function(name = NULL, map = NULL, center = NULL, radius = NULL, polygon = NULL) {
  if (is.null(name)) name <- "unnamed region"
  region <- sf::st_sf(
    region = name,
    geometry = define_boundary(map, center, radius, polygon)
  ) %>% sf::st_make_valid()
  sf::st_agr(region) <- "constant"

  # keep the map as an internal attribute
  attr(region, "map") <- map

  class(region) <- set_class(region, "region")
  region
}


#' Reproject coordinates between coordinate systems
#'
#' Converts between coordinates on a compiled raster map (i.e. pixel
#' units) and different Geographic Coordinate Systems (CRS).
#'
#' @param x,y Coordinates in two dimensions (if missing, coordinates
#'   are expected to be in the \code{data.frame} specified in the
#'   \code{coords} parameter as columns "x" and "y")
#' @param from,to Either a CRS code accepted by GDAL, a valid integer
#'   EPSG value, an object of class \code{crs}, the value "raster"
#'   (converting from/to pixel coordinates), or "world" (converting
#'   from/to whatever CRS is set for the underlying map)
#' @param coords data.frame-like object with coordinates in columns
#'   "x" and "y"
#' @param model Object of the class \code{slendr_model}
#' @param add Add column coordinates to the input data.frame
#'   \code{coords} (coordinates otherwise returned as a separate
#'   object)?
#' @param input_prefix,output_prefix Input and output prefixes of data
#'   frame columns with spatial coordinates
#'
#' @return Data.frame with converted two-dimensional coordinates
#'
#' @export
reproject <- function(from, to, x = NULL, y = NULL, coords = NULL, model = NULL,
                      add = FALSE, input_prefix = "", output_prefix = "new") {
  if ((is.null(x) | is.null(y)) & is.null(coords))
    stop("Coordinates for conversion are missing", call. = FALSE)

  if ((from == "raster" | to == "raster") & is.null(model))
    stop("Model object needs to be specified for conversion of raster coordinates", call. = FALSE)

  if (add & is.null(coords))
    stop("Converted coordinates can only be added to a provided data.frame", call. = FALSE)

  inx <- paste0(input_prefix, "x"); iny <- paste0(input_prefix, "y")
  outx <- paste0(output_prefix, "x"); outy <- paste0(output_prefix, "y")
  if (!is.null(coords) & !all(c(inx, iny) %in% colnames(coords)))
    stop("Columns '", inx, "' and '", iny, "' must be present in the input data.frame", call. = FALSE)

  if (!is.null(model)) {
    # dimension of the map in the projected CRS units
    bbox <- sf::st_bbox(model$world)
    map_dim <- c(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])

    # dimension of the rasterized map in pixel units
    # (x/y dimensions of PNGs are reversed)
    raster_dim <- dim(png::readPNG(file.path(model$path, model$maps$path[1])))[2:1]
  }

  if (to == "world") to <- sf::st_crs(model$world)
  if (from == "world" && has_crs(model$world)) from <- sf::st_crs(model$world)

  if (is.null(coords)) {
    df <- data.frame(x = x, y = y)
    colnames(df) <- c(inx, iny)
  } else
    df <- coords[, c(inx, iny)]

  if (from == "raster") {
    # convert pixel coordinates to na sf object in world-based coordinates
    df[[inx]] <- bbox["xmin"] + map_dim[1] * df[[inx]] / raster_dim[1]
    df[[iny]] <- bbox["ymin"] + map_dim[2] * df[[iny]] / raster_dim[2]
    point <- sf::st_as_sf(df, coords = c(inx, iny), crs = sf::st_crs(model$world))
  } else {
    # ... otherwise create a formal sf point object from the
    # coordinates already given
    point <- sf::st_as_sf(df, coords = c(inx, iny), crs = from)
  }

  if (to == "raster") {
    if (has_crs(model$world))
      point<- sf::st_transform(point, crs = sf::st_crs(model$world))
    point_coords <- sf::st_coordinates(point)
    newx <- abs((point_coords[, "X"] - bbox["xmin"])) / map_dim[1] * raster_dim[1]
    newy <- abs((point_coords[, "Y"] - bbox["ymin"])) / map_dim[2] * raster_dim[2]
    new_point <- data.frame(newx = round(as.vector(newx)), newy = round(as.vector(newy)))
    colnames(new_point) <- c(outx, outy)
  } else if (!is.na(to)) {
    new_point <- sf::st_transform(point, crs = to) %>% sf::st_coordinates()
    colnames(new_point) <- c(outx, outy)
  } else {
    new_point <- point %>% sf::st_coordinates()
    colnames(new_point) <- c(outx, outy)
  }

  # if (nrow(new_point) == 1)
  #   return(as.vector(unlist(new_point)))

  if (add) new_point <- cbind(coords, new_point) %>% dplyr::as_tibble()

  new_point
}


#' Merge two spatial \code{slendr} objects into one
#'
#' @param x Object of the class \code{slendr}
#' @param y Object of the class \code{slendr}
#' @param name Optional name of the resulting geographic region. If missing,
#'   name will be constructed from the function arguments.
#'
#' @return Object of the class \code{slendr_region}
#'
#' @export
#'
#' @example man/examples/spatial_functions.R
join <- function(x, y, name = NULL) {
  if (!inherits(x, "slendr")) x <- region(polygon = x)
  if (!inherits(y, "slendr")) y <- region(polygon = y)

  result <- sf::st_union(x, y)
  result$region.1 <- NULL
  if (is.null(name))
    result$region <- sprintf("(%s and %s)", x$region, y$region)
  else
    result$region <- name
  attrs <- if (!is.null(attr(x, "map"))) "map" else NULL
  result <- copy_attributes(result, x, attrs)
  sf::st_agr(result) <- "constant"
  result
}


#' Generate the overlap of two \code{slendr} objects
#'
#' @inheritParams join
#'
#' @return Object of the class \code{slendr_region}
#'
#' @export
overlap <- function(x, y, name = NULL) {
  if (!inherits(x, "slendr")) x <- region(polygon = x)
  if (!inherits(y, "slendr")) y <- region(polygon = y)

  result <- sf::st_intersection(x, y)

  if (nrow(result)) {
    if (nrow(result) > 1)
      result <- sf::st_sf(geometry = sf::st_combine(result))

    if (is.null(name)) {
      xname <- deparse(substitute(x))
      yname <- deparse(substitute(y))
      result$region <- sprintf("(overlap of %s and %s)", xname, yname)
    } else
      result$region <- name
    result <- result[, c("region", "geometry")]
  }

  map <- attr(x, "map")

  class(result) <- set_class(result, "region")
  attr(result, "map") <- map
  sf::st_agr(result) <- "constant"

  result
}


#' Generate the difference between two \code{slendr} objects
#'
#' @inheritParams join
#'
#' @return Object of the class \code{slendr_region}
#'
#' @export
subtract <- function(x, y, name = NULL) {
  if (!inherits(x, "slendr")) x <- region(polygon = x)
  if (!inherits(y, "slendr")) y <- region(polygon = y)

  result <- sf::st_difference(x, y)

  if (nrow(result)) {
    if (is.null(name)) {
      xname <- deparse(substitute(x))
      yname <- deparse(substitute(y))
      result$region <- sprintf("(%s minus %s)", xname, yname)
    } else
      result$region <- name
  }

  map <- attr(x, "map")

  result <- result[, c("region", "geometry")]
  class(result) <- set_class(result, "region")
  attr(result, "map") <- map
  sf::st_agr(result) <- "constant"

  result
}


#' Calculate the distance between a pair of spatial boundaries
#'
#' @param x,y Objects of the class \code{slendr}
#' @param measure How to measure distance? This can be either \code{'border'}
#'   (distance between the borders of \code{x} and \code{y}) or \code{'center'}
#'   (distance between their centroids).
#' @param time Time closest to the spatial maps of \code{x} and \code{y} if they
#'   represent \code{slendr_pop} population boundaries (ignored for general
#'   \code{slendr_region} objects)
#'
#' @return If the coordinate reference system was specified, a distance in
#'   projected units (i.e. meters) is returned. Otherwise the function returns a
#'   normal Euclidian distance.
#'
#' @export
distance <- function(x, y, measure, time = NULL) {
  if (!measure %in% c("center", "border"))
    stop("Unknown distance measure method provided (must be either 'center' or 'border')", call. = FALSE)

  if ((inherits(x, "slendr_pop") | inherits(y, "slendr_pop")) & is.null(time))
    stop("Time of the nearest spatial snapshot must be be given when calculating
distance between population boundaries", call. = FALSE)

  if (inherits(x, "slendr_pop")) x <- x[which.min(abs(x$tmap - time)), ]
  if (inherits(y, "slendr_pop")) y <- y[which.min(abs(y$tmap - time)), ]

  if (measure == "center") {
    x <- sf::st_centroid(x)
    y <- sf::st_centroid(y)
  }

  as.vector(sf::st_distance(x, y))
}


#' Calculate the area covered by the given slendr object
#'
#' @param x Object of the class \code{slendr}
#'
#' @return Area covered by the input object. If a \code{slendr_pop}
#'   was given, a table with an population range area in each time
#'   point will be returned. If a \code{slendr_region} or
#'   \code{slendr_world} object was specified, the total area covered
#'   by this object's spatial boundary will be returned.
#'
#' @export
area <- function(x) {
  if (!inherits(x, "slendr") & !inherits(x, "sf"))
    stop("Input must be of the type 'slendr' or 'sf'", call. = FALSE)

  if (inherits(x, "slendr_pop")) {
    areas <- purrr::map_dbl(seq_len(nrow(x)), ~ as.numeric(sum(sf::st_area(x[.x, ]))))
    times <- x$tmap
    return(dplyr::tibble(time = times, area = areas))
  } else if (inherits(x, "slendr_map") || inherits(x, "slendr_region"))
    return(as.numeric(sum(sf::st_area(x))))
  else
    stop("Only the areas covered by slendr_(pop/region/world) objects can be computed",
         call. = FALSE)
}


#' Return the dimensions of the world map
#'
#' @param map Object of the type \code{slendr_map}
#' @param original Return dimensions in the original coordinate system (CRS)
#'   instead of the internal projected CRS?
#'
#' @return If the coordinate reference system was specified, the function
#'   returns a two-dimensional vector of the world dimensions in the projected
#'   units. Otherwise the dimensions of the two-dimensional abstract plane are
#'   returned.
#'
#' @export
dimensions <- function(map, original = FALSE) {
  if (!inherits(map, "slendr_map"))
    stop("Incorrect input type. Object of the type 'slendr_map' expected", call. = FALSE)
  if (has_crs(map) && original)
    return(c(diff(attr(map, "xrange")), diff(attr(map, "yrange"))))
  else
    c(as.vector(diff(sf::st_bbox(map)[c("xmin", "xmax")])),
      as.vector(diff(sf::st_bbox(map)[c("ymin", "ymax")])))
}


#' Define sampling events for a given set of populations
#'
#' Schedule sampling events at specified times and, optionally, a given set of
#' locations on a landscape
#'
#' If both times and locations are given, the the sampling will be scheduled on
#' each specified location in each given time-point. Note that for the
#' time-being, in the interest of simplicity, no sanity checks are performed on
#' the locations given except the restriction that the sampling points must fall
#' within the bounding box around the simulated world map. Other than that,
#' slendr will simply instruct its SLiM backend script to sample individuals as
#' close to the sampling points given as possible, regardless of whethere those
#' points lie within a population spatial boundary at that particular moment of
#' time.
#'
#' @param model Object of the class \code{slendr_model}
#' @param times Integer vector of times (in model time units) at which to
#'   schedule remembering of individuals in the tree-sequence
#' @param ... Lists of two elements (\code{slendr_pop} population object-<number
#'   of individuals to sample), representing from which populations should how
#'   many individuals be remembered at times given by \code{times}
#' @param locations List of vector pairs, defining two-dimensional coordinates
#'   of locations at which the closest number of individuals from given
#'   populations should be sampled. If \code{NULL} (the default), individuals
#'   will be sampled randomly throughout their spatial boundary.
#' @param strict Should any occurence of a population not being present at a
#'   given time result in an error? Default is \code{FALSE}, meaning that
#'   invalid sampling times for any populations will be quietly ignored.
#'
#' @return Data frame with three columns: time of sampling, population to sample
#'   from, how many individuals to sample
#'
#' @export
sampling <- function(model, times, ..., locations = NULL, strict = FALSE) {
  if (!inherits(model, "slendr_model"))
    stop("A slendr_model object must be specified", call. = FALSE)

  times <- unique(as.integer(sort(times)))

  samples <- list(...)
  sample_pops <- purrr::map(samples, 1)
  sample_counts <- purrr::map(samples, 2)

  if (length(sample_pops) != length(sample_counts))
    stop("Samples must be represented by pairs of <slendr_pop>-<n>", call. = FALSE)

  if (!all(purrr::map_lgl(sample_pops, ~ inherits(.x, "slendr_pop"))))
    stop("Objects to sample from must be of the class 'slendr_pop'", call. = FALSE)

  if (!all(purrr::map_lgl(sample_counts, ~ .x == round(.x))))
    stop("Sample counts must be integer numbers", call. = FALSE)

  # make sure that all sampling times fall in the time window of the simulation itself
  split_times <- purrr::map_int(model$populations, ~ attr(., "history")[[1]]$time)
  if ((model$direction == "forward" && (any(times > min(split_times) + model$orig_length)
                                        || any(times < min(split_times)))) ||
      (model$direction == "backward" && (any(times < max(split_times) - model$orig_length)
                                         || any(times > max(split_times))))) {

    if (strict)
      stop("A sampling event was scheduled outside of the simulation time window", call. = FALSE)
    else
      times <- times[times <= model$orig_length]
  }

  schedule <- purrr::map_dfr(times, function(t) {
    purrr::map_dfr(samples, function(s) {
      pop <- s[[1]]
      n <- s[[2]]
      tryCatch(
        {
          check_removal_time(t, pop, direction = model$direction)
          check_present_time(t, pop, direction = model$direction, offset = model$generation_time)
          if (!is.infinite(n)) n <- as.integer(n)
          dplyr::tibble(time = t, pop = pop$pop[1], n = n)
        },
        error = function(cond) {
          if (!strict)
            return(NULL)
          else
            stop("Cannot schedule sampling for '", pop$pop, "' at time ", t,
                 " because the population will not yet be present in the simulation",
                 " at that point. Consider running this function with `strict = FALSE`",
                 " which will automatically retain only valid sampling events.",
                 call. = FALSE)
        })
    })
  })

  if (is.null(schedule))
    stop("No sampling events have been generated", call. = FALSE)

  if (!nrow(schedule)) {
    warning("No valid sampling events were retained", call. = FALSE)
    return(NULL)
  }

  if (!is.null(locations)) {
    check_location_bounds(locations, model$world)

    # convert the list of coordinate pairs into a data frame with x and y
    # columns transformed from world-based coordinates into raster-based
    # coordinates
    locations_df <- dplyr::tibble(
      orig_x = purrr::map_dbl(locations, ~ .[[1]]),
      orig_y = purrr::map_dbl(locations, ~ .[[2]])
    ) %>%
      reproject(
        coords = ., model = model,
        from = "world", to = "raster",
        input_prefix = "orig_", output_prefix = "",
        add = TRUE
      ) %>%
      dplyr::rename(x_orig = orig_x, y_orig = orig_y)

    schedule <- merge(schedule, locations_df)
  } else {
    schedule$x <- schedule$y <- schedule$x_orig <- schedule$y_orig <- NA
  }

  schedule
}


# Internal implementation of expand() and shrink() functions
shrink_or_expand <- function(pop, by, end, start, overlap, snapshots, polygon,
                             lock, verbose) {
  check_event_time(c(start, end), pop)
  check_removal_time(start, pop)
  check_removal_time(end, pop)

  map <- attr(pop, "map")

  # get the last active population size
  prev_N <- sapply(attr(pop, "history"), function(event) event$N) %>%
    Filter(Negate(is.null), .) %>%
    unlist %>%
    utils::tail(1)

  # get the last available population boundary
  region_start <- pop[nrow(pop), ]
  sf::st_agr(region_start) <- "constant"

  if (is.null(snapshots)) {
    n <- 1
    message("Iterative search for the minimum sufficient number of intermediate
spatial snapshots, starting at ", n, ". This should only take a couple of
seconds, but if you don't want to wait, you can set `snapshots = N` manually.")
  } else
    n <- snapshots

  # iterate through the number of intermediate spatial boundaries to reach
  # the required overlap between subsequent spatial maps
  repeat {
    times <- seq(start, end, length.out = n + 1)[-1]

    # generate intermediate spatial maps, starting from the last one
    inter_regions <- list()
    inter_regions[[1]] <- region_start
    for (i in seq_along(times)) {
      exp_region <- sf::st_buffer(inter_regions[[1]], dist = i * (by / n))
      exp_region$tmap <- times[i]
      sf::st_agr(exp_region) <- "constant"

      # restrict the next spatial boundary to the region of interest
      if (!is.null(polygon)) {
        if (!inherits(polygon, "slendr_region"))
          polygon <- region(polygon = polygon, map = map)
        exp_region <- sf::st_intersection(exp_region, polygon)
        exp_region$region <- NULL
        sf::st_agr(exp_region) <- "constant"
      }

      inter_regions[[i + 1]] <- exp_region
    }

    if (!is.null(snapshots)) break

    # if the boundary is supposed to be shrinking, the order of spatial maps
    # must be reversed in order to check the amount of overlap
    direction <- ifelse(by < 0, rev, identity)
    overlaps <- compute_overlaps(do.call(rbind, direction(inter_regions)))

    if (all(overlaps >= overlap)) {
      message("The required ", sprintf("%.1f%%", 100 * overlap),
              " overlap between subsequent spatial maps has been met")
      break
    } else {
      n <- n + 1
      if (verbose)
        message("- Increasing to ", n, " snapshots")
    }
  }

  all_maps <- do.call(rbind, inter_regions[-1]) %>% rbind(pop, .)
  sf::st_agr(all_maps) <- "constant"

  result <- copy_attributes(
    all_maps, pop,
    c("map", "parent", "remove", "intersect", "aquatic", "history")
  )

  start_area <- sf::st_area(utils::head(inter_regions, 1)[[1]])
  end_area <- sf::st_area(utils::tail(inter_regions, 1)[[1]])
  action <- ifelse(start_area < end_area, "expand", "contract")

  attr(result, "history") <- append(attr(result, "history"), list(data.frame(
    pop =  unique(region_start$pop),
    event = action,
    tstart = start,
    tend = end
  )))


  if (lock) {
    areas <- as.numeric(sapply(inter_regions, sf::st_area))
    area_changes <- areas[-1] / areas[-length(areas)]
    new_N <- round(cumprod(area_changes) * prev_N)
    changes <- data.frame(
      pop =  unique(pop$pop),
      event = "resize",
      how = "step",
      N = new_N,
      prev_N = c(prev_N, new_N[-length(new_N)]),
      tresize = sapply(inter_regions[-1], `[[`, "tmap"),
      tend = NA
    )
    attr(result, "history") <- append(attr(result, "history"), list(changes))
    # for (i in seq_along(inter_regions)[-1]) {
    #   time <- inter_regions[[i]]$tmap
    #   result <- resize(result, N = new_N[i - 1], time = time, how = "step")
    # }
  }

  result
}

#' Get split time of the given population
#'
#' @param pop Object of the class \code{slendr_pop}
#'
#' @return Split time of the population
#' @export
#'
#' @examples
#' pop <- population("pop1", N = 1000, time = 42)
#' split_time(pop)
split_time <- function(pop) attr(pop, "history")[[1]]$time

#' Setup a dedicated Python virtual environment for slendr
#'
#' @param env Either a name of a conda environment, or a path to a standard
#'   Python virtual environment (such as one created by \code{python3 -m venv
#'   <path to an environment>}) with necessary Python dependencies (msprime,
#'   tskit, pyslim, and pandas). If \code{NULL} (the default), the user will be
#'   offered to install and configure an entirely isolated Miniconda Python
#'   environment just for slendr. If this environment is already present,
#'   calling \code{setup_env()} without the \code{env} argument will simply
#'   activate this environment.
#' @param quiet Should informative messages be printed to the console? Default
#'   is \code{FALSE}.
#'
#' @export
setup_env <- function(env = NULL, quiet = FALSE) {
  if (!is.null(env)) {
    # check for the presence of a regular virtual environment (if the user
    # provided their own environment)
    env_type <- tryCatch(
      {
        reticulate::use_virtualenv(env, required = TRUE)
        "virtualenv"
      },
      error = function(cond) FALSE
    )

    # if a regular venv is missing, check for the conda environment
    if (env_type != "virtualenv") {
      env_type <- tryCatch(
        {
          reticulate::use_condaenv(env, required = TRUE)
          "conda"
        },
        error = function(cond) FALSE
      )
    }

    if (env_type %in% c("virtualenv", "conda")) {
      if (!quiet)
        message("Successfully connected to the specified ",
                ifelse(env_type == "virtualenv", "Python", "conda"),
                " virtual environment '", env, "'")
      # check if all Python dependencies are present in the activated environment
      if (!reticulate::py_module_available("msprime") && ask_install("msprime"))
        reticulate::py_install("msprime=1.1.0", envname = env)
      if (!reticulate::py_module_available("tskit") && ask_install("tskit"))
        reticulate::py_install("tskit=0.4.1", envname = env)
      if (!reticulate::py_module_available("pyslim") && ask_install("pyslim"))
        reticulate::py_install("pyslim=0.700", envname = env)
      if (!reticulate::py_module_available("pandas") &&
          ask_install("pandas (needed for the msprime back end script)"))
        reticulate::py_install("pandas=1.3.5", envname = env)

      has_tskit <- reticulate::py_module_available("tskit")
      has_msprime <- reticulate::py_module_available("msprime")
      has_pyslim <- reticulate::py_module_available("pyslim")

      if (!all(c(has_tskit, has_pyslim, has_msprime)))
        stop("Python modules tskit, msprime, and pyslim are required for tree sequence\n",
             "analysis with slendr. If you're having trouble setting up the Python ",
             "environment,\nyou can run `setup_env()` without any arguments and ",
             "slendr with configure\neverything for you automatically.", call. = FALSE)
    } else
      stop("The specified Python environment '", env, "' not found among\n",
           "regular Python virtual environments or conda environments.\n\n",
           "Note: If you run setup_env() without any arguments, slendr will set up",
           "\nyour Python environment for you and configure it automatically.",
           call. = FALSE)
  } else if ("automatic_slendr_python_env" %in% reticulate::conda_list()$name) {
    reticulate::use_condaenv("automatic_slendr_python_env", required = TRUE)
    if (!reticulate::py_module_available("msprime") ||
        !reticulate::py_module_available("tskit") ||
        !reticulate::py_module_available("pyslim")) {
      stop("Python environment 'automatic_slendr_python_env' has been found but it",
           " does not appear to have msprime, tskit and pyslim modules all",
           " installed. Perhaps the environment got corrupted somehow?",
           " Running `clear_env()` and `setup_env()` to reset the slendr's Python",
           " environment is recommended.")
    } else if (!quiet)
      message("The slendr interface to required Python modules ",
              "has been successfully activated.")
  } else {
    answer <- utils::menu(
      c("No", "Yes"),
      title = paste0(
        "No pre-configured Python environment for slendr has been found.\n\n",
        "Do you wish to install a Miniconda Python distribution and create\n",
        "an isolated environment with all required Python modules automatically?\n",
        "\n(No need to worry, everything will be installed into a completely\n",
        "separate location into an isolated environment. This won't affect\n",
        "your system or your other Python installations at all.)\n\n",
        "If your answer is \"no\", you can set up your own virtual environment\n",
        "with Python >= 3.8, msprime >= 1.1.0, tskit >= 0.4.1, pyslim >= 0.700,\n",
        "and pandas and provide it to the setup_env() function using its\n",
        "`env` argument (see `?setup_env` for more detail).\n\n",
        "Do you wish to setup a Python virtual environment just for slendr and\n",
        "populate it with the required Python modules?")
      )
    if (answer == 2) {
      if (!dir.exists(reticulate::miniconda_path()))
        reticulate::install_miniconda()

      reticulate::conda_create(
        packages = c("msprime=1.1.0", "tskit=0.4.1", "pyslim=0.700", "pandas=1.3.5"),
        envname = "automatic_slendr_python_env"
      )

      reticulate::use_condaenv("automatic_slendr_python_env", required = TRUE)

      if (!quiet)
        message("Python environment for slendr has been successfuly created, and ",
                "the R\ninterface to msprime, tskit, and pyslim modules has been activated. ",
                "In\nthe future, you may simply call setup_env() again and slendr ",
                "will activate\nthis environment automatically on its own.")
    } else
      warning("Your Python environment is not set up correctly which means that the tree\n",
              "sequence functionality of slendr will not work.", call. = FALSE)
  }
}

#' Remove the automatically created slendr Python environment
#'
#' @param force Ask before deleting the environment?
#'
#' @export
clear_env <- function(force = FALSE) {
  if ("automatic_slendr_python_env" %in% reticulate::conda_list()$name) {
    answer <- utils::menu(
      c("No", "Yes"),
      title = paste0(
        "Are you sure you want to delete the automatically created slendr ",
        "Python\nenvironment? If you do, you can create it again by running",
        "`setup_env()`\nwithout any arguments in a new R session."
      )
    )
    if (answer == 2) reticulate::conda_remove("automatic_slendr_python_env")
    message("The slendr Python environment has been sucessfully removed.")
  } else
    stop("No conda environment named 'automatic_slendr_python_env' has been found",
         call. = FALSE)
}

#' Check that the active Python environment is correctly setup for slendr
#'
#' This function inspects the Python environment which has been activated by the
#' reticulate package and prints the versions of all slendr Python dependencies
#' to the console.
#'
#' @export
check_env <- function() {
  # if there is no Python available on user's system, don't immediately
  # jump to installing miniconda (let's deal with that in setup_env())
  orig_env <- Sys.getenv("RETICULATE_MINICONDA_ENABLED")
  Sys.setenv(RETICULATE_MINICONDA_ENABLED = FALSE)
  on.exit(Sys.setenv(RETICULATE_MINICONDA_ENABLED = orig_env))

  py <- reticulate::py_discover_config()

  has_tskit <- reticulate::py_module_available("tskit")
  has_msprime <- reticulate::py_module_available("msprime")
  has_pyslim <- reticulate::py_module_available("pyslim")
  # has_pylib <- !is.null(pylib)

  if (has_tskit)
    tskit_version <- paste("version", tskit[["_version"]]$tskit_version, "\u2713")
  else
    tskit_version <- "MISSING \u274C"

  if (has_msprime)
    msprime_version <- paste("version", msp[["_version"]]$version, "\u2713")
  else
    msprime_version <- "MISSING \u274C"

  if (has_pyslim)
    pyslim_version <- paste("version", pyslim$pyslim_version, "\u2713")
  else
    pyslim_version <- "MISSING \u274C"

  # if (has_pylib)
  #   pylib_status <- "successfully loaded \u2713"
  # else
  #   pylib_status <- "NOT LOADED \u274C"

  cat("Summary of the currently active Python environment:\n\n")
  cat("Python binary:", py$python, "\n")
  cat("Python version:", py$version_string, "\n")

  cat("\nslendr requirements:\n")
  cat(" - tskit:", tskit_version, "\n")
  cat(" - msprime:", msprime_version, "\n")
  cat(" - pyslim:", pyslim_version, "\n")
  # cat(" - slendr module:", pylib_status, "\n")

  if (!all(c(has_tskit, has_pyslim, has_msprime)))
    cat("\nNote that due to the technical limitations of embedded Python,",
        "if you\nwant to switch to another Python environment you will need",
        "to restart\nyour R session first.\n")
    # reference: https://github.com/rstudio/reticulate/issues/27#issuecomment-512256949
}
