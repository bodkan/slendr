#' Define a population range
#'
#' @param name Name of the population
#' @param time Time of the population appearance
#' @param N Number of individuals at the time of split
#' @param parent Parent population object or "ancestor" character scalar
#' @param map Object of the type \code{sf} which defines the map
#' @param center Vector of two elements defining a center of a circular range
#' @param radius Scalar defining a radius of a range in kilometers
#' @param coords List of vector pairs, defining corners of the range
#' @param region Geographic region of the class \code{spannr_region}
#' @param remove Time at which the population should be removed
#' @param intersect Intersect the population's boundaries with landscape
#'   features?
#'
#' @return Object of the \code{spannr_pop} (and \code{sf}) class
#'
#' @export
population <- function(name, parent, N, time = NULL, map = NULL,
                       center = NULL, radius = NULL, coords = NULL,
                       region = NULL, remove = NULL, intersect = TRUE) {
  # is this the first population defined in the model?
  if (is.character(parent) && parent == "ancestor") {
    time <- Inf
    if (is.null(map))
      stop("Ancestral population must specify its 'map'", call. = FALSE)
  } else if (!is.character(parent) & is.null(time)) {
    stop("Split time of each population (except for the ancestral population) must be specified", call. = FALSE)
  } else {
    map <- attr(parent, "map")
  }
  # define the population range as a simple geometry object
  # and bind it with the annotation info into an sf object
  if (!is.null(region) & is.null(center)) {
    if (sf::st_crs(map) != sf::st_crs(region))
      stop("The specified region uses a different coordinate system than the implied world", call. = FALSE)
    range <- sf::st_sfc(sf::st_geometry(region))
  } else {
    range <- spatial_range(map, center, radius, coords)
  }
  pop_range <- sf::st_sf(
    data.frame(pop = name, time = time, N = N, stringsAsFactors = FALSE),
    geometry = range
  )

  sf::st_agr(pop_range) <- "constant"

  # when to clean up the population?
  attr(pop_range, "remove") <- ifelse(!is.null(remove), remove, -1)

  # keep a record of the parent population
  if (inherits(parent, "spannr_pop")) {
    attr(pop_range, "parent") <- parent[nrow(parent), ]
    # keep the map as an internal attribute
    attr(pop_range, "map") <- map
  } else if (is.character(parent) & parent == "ancestor") {
    attr(pop_range, "parent") <- "ancestor"
    attr(pop_range, "map") <- map
  } else
    stop("Suspicious parental population specified", call. = FALSE)

  attr(pop_range, "intersect") <- intersect

  class(pop_range) <- set_class(pop_range, "pop")
  pop_range
}

#' Add a new time snapshot for an already defined population
#'
#' This function allows a more manual control of spatial map changes in addition
#' to the \code{expand} and \code{move} functions
#'
#' @param pop Population object of the \code{spannr} class
#' @param time Time of the current snapshot that is being defined
#' @param N Effective population size (stays the same by default)
#' @param center Vector of two elements defining a center of a circular range
#' @param radius Scalar defining a radius of a range in kilometers
#' @param coords List of vector pairs, defining corners of the range
#' @param region Geographic region of the class \code{spannr_region}
#'
#' @return Object of the \code{spannr_pop} (and \code{sf}) class
#'
#' @export
change <- function(pop, time, N = NULL,
                   center = NULL, radius = NULL, coords = NULL,
                   region = NULL) {
  if (time %in% pop$time)
    stop("Time point already defined", call. = FALSE)

  if (time > pop[nrow(pop), ]$time)
    warning("Specifying a spatial map at a time point prior to the last spatial map present for the population",
            call. = FALSE)

  if (time < attr(pop, "remove"))
    stop("Cannot update population status after it has been removed")

  map <- attr(pop, "map")
  # define the new population range or re-use the old one
  if (!is.null(region)) {
    range <- sf::st_sfc(sf::st_geometry(region))
  } else if (!is.null(center) & !is.null(radius)){
    range <- spatial_range(map, center, radius, coords)
  } else
    range <- sf::st_geometry(pop[nrow(pop), ])

  if (is.null(N)) N <- pop[nrow(pop), ]$N

  updated <- sf::st_sf(
    data.frame(pop = unique(pop$pop), time = time, N = N, stringsAsFactors = FALSE),
    geometry = range
  )

  res <- rbind(pop, updated)

  class(res) <- class(pop)
  attr(res, "parent") <- attr(pop, "parent")
  attr(res, "remove") <- attr(pop, "remove")
  attr(res, "intersect") <-attr(pop, "intersect")
  attr(res, "map") <- map
  sf::st_agr(res) <- "constant"

  res
}


#' Expand population radius by a given factor in a given time
#'
#' @param pop Spatial population object of \code{spannr_pop} class
#' @param by How many kilometers to expand by?
#' @param start,end When does the spatial population expansion start/end?
#' @param snapshots Number of time slices to split the movement into
#' @param region Geographic region to restrict the expansion to
#'
#' @return Object of the \code{spannr_pop} (and \code{sf}) class
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
    exp_region <- sf::st_buffer(inter_regions[[1]], dist = i * (by / snapshots))
    exp_region$time <- times[i]
    inter_regions[[i + 1]] <- exp_region
  }

  inter_regions <- rbind(pop, do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the map as an internal attribute
  attr(inter_regions, "map") <- attr(pop, "map")
  # propagate the information about the parental population
  attr(inter_regions, "parent") <- attr(pop, "parent")
  # optionally, add a movement boundary
  attr(inter_regions, "region") <- region
  # retain the cleanup time
  attr(inter_regions, "remove") <- attr(pop, "remove")
  # retain information whether to intersect the population boundaries
  attr(inter_regions, "intersect") <-attr(pop, "intersect")

  class(inter_regions) <- set_class(inter_regions, "pop")
  inter_regions
}


#' Move population to a new location in a given amount of time
#'
#' @param pop Spatial population object of \code{spannr_pop} class
#' @param trajectory List of two-dimensional vectors [(longitude, latitude)]
#'   specifying the trajectory of the population movement
#' @param start,end Start/end points of the population movement
#' @param snapshots Number of time slices to split the movement into
#'
#' @return Object of the \code{spannr_pop} (and \code{sf}) class
#'
#' @export
move <- function(pop, trajectory, end, snapshots, start = NULL) {
  check_not_intersected(pop)
  
  map <- attr(pop, "map")

  # take care of just a single destination point being specified
  if (!is.list(trajectory) & length(trajectory) == 2)
    trajectory <- list(trajectory)

  region_start <- pop[nrow(pop), ]
  if (!is.null(start)) {
    region_start$time <- start
    sf::st_agr(region_start) <- "constant"
  }
  start_time <- region_start$time

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
    traj <- sf::st_sf(traj, crs = source_crs) %>% sf::st_transform(crs = target_crs)
  } else {
    traj <- sf::st_sf(traj)
  }

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
        N = region_start$N,
        stringsAsFactors = FALSE
      ),
      geometry = shifted_region,
      crs = sf::st_crs(inter_regions[[i]])
    )
  }

  inter_regions <- rbind(pop, do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the map as an internal attribute
  attr(inter_regions, "map") <- map
  # propagate the information about the parental population
  attr(inter_regions, "parent") <- attr(pop, "parent")
  # retain the cleanup time
  attr(inter_regions, "remove") <- attr(pop, "remove")
  # retain information whether to intersect the population boundaries
  attr(inter_regions, "intersect") <-attr(pop, "intersect")

  class(inter_regions) <- set_class(inter_regions, "pop")

  inter_regions
}


#' Define a geographic region
#'
#' @param name Name of the geographic region
#' @param map Object of the type \code{sf} which defines the map
#' @param coords List of vector pairs, defining corners of the range
#'
#' @return Object of the \code{spannr_region} (and \code{sf}) class
#'
#' @export
region <- function(name, map, coords) {
  region <- sf::st_sf(
    region = name,
    geometry = spatial_range(map, coords = coords)
  )
  sf::st_agr(region) <- "constant"

  # keep the map as an internal attribute
  attr(region, "map") <- map

  class(region) <- set_class(region, "region")
  region
}

#' Define a world map for all spatial operations
#'
#' Download Natural Earth land area map data, zoom on a given window
#' determined by longitude and latitude coordinates and transform to a
#' specified projection. Alternatively, load a previously downloaded
#' and unzipped data such as the "Land polygons" data here:
#' <https://www.naturalearthdata.com/downloads/110m-physical-vectors/>
#'
#' @param xrange Numeric vector with minimum and maximum longitude
#' @param yrange Numeric vector with minimum and maximum latitude
#' @param landscape Either "blank" (for blank abstract geography),
#'   "naturalearth" (for real Earth geography) or an object of the
#'   class \code{sf} defining abstract geographic features of the
#'   world
#' @param crs EPSG code of a coordinate reference system to use for
#'   spatial operations (no CRS assumed by default, implying an
#'   abstract landscape not tied to any real-world geographic region)
#' @param ne_dir Path to the directory where Natural Earth data was
#'   manually downloaded and unzipped (used only when \code{landscape}
#'   is "naturalearth")
#'
#' @return Object of the class \code{spannr_map}
#'
#' @export
world <- function(xrange, yrange, landscape = "naturalearth", crs = NULL, ne_dir = NULL) {
   if (inherits(landscape, "sf")) { # a landscape defined by the user
     map <- sf::st_sf(geometry = sf::st_as_sf(landscape)) %>%
       set_bbox(xmin = xrange[1], xmax = xrange[2], ymin = yrange[1], ymax = yrange[2])
   } else if (landscape == "blank") { # an empty abstract landscape
    map <- sf::st_sf(geometry = sf::st_sfc()) %>%
       set_bbox(xmin = xrange[1], xmax = xrange[2], ymin = yrange[1], ymax = yrange[2])
  } else if (landscape == "naturalearth") {  # Natural Earth data vector landscape
    scale <- "small"
    type <- "land"
    category <- "physical"

    if (is.null(ne_dir)) {
      map_raw <- rnaturalearth::ne_download(scale, type, category, returnclass = "sf")
    } else {
      map_raw <- rnaturalearth::ne_load(scale, type, category,
                                        returnclass = "sf", destdir = ne_dir)
    }
    sf::st_agr(map_raw) <- "constant"

    ## transform the map (default geographic CRS) into the target CRS
    map_transf <- sf::st_transform(map_raw, crs)

    ## define boundary coordinates in the target CRS
    zoom_bounds <- define_zoom(xrange, yrange, "EPSG:4326")
    zoom_transf <- sf::st_transform(zoom_bounds, crs)

    ## crop the map to the boundary coordinates
    map <- sf::st_crop(map_transf, zoom_transf)
  } else {
    stop("Landscape has to be either 'abstract', 'ne' (Natural Earth) or an object of the class 'sf'", call. = FALSE)
  }

  sf::st_agr(map) <- "constant"

  class(map) <- set_class(map, "map")
  attr(map, "xrange") <- xrange
  attr(map, "yrange") <- yrange

  map
}


#' Define an admixture event
#'
#' @param from,to Population range objects of the class \code{spammr_pop}
#' @param rate Scalar value in the range (0, 1] specifying the proportion of
#'   migration over given time period
#' @param start,end Start and end of the admixture event
#' @param overlap Require spatial overlap between admixing populations (default
#'   \code{TRUE})
#'
#' @return Object of the class data.frame
#'
#' @export
admixture <- function(from, to, rate, start, end, overlap = TRUE) {
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

  if (overlap & area_overlap == 0) {
    stop(sprintf("
No overlap between population ranges of %s and %s at time %d.

Please check the spatial maps of both populations by running
`plot(%s, %s, pop_facets = F)` and adjust them accordingly.
Alternatively, in case this makes sense for your model, you can
add `overlap = F` which will instruct spannr to simulate admixture
without spatial overlap between populations.",
      from_name, to_name, start, deparse(substitute(from)),
      deparse(substitute(to)), call. = FALSE))
  }

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


#' Convert between coordinate system (raster-coordinates or CRS)
#'
#' @param x,y Coordinates in two dimensions (if missing, coordinates
#'   expected in the \code{coords} parameter as columns "x" and "y")
#' @param from,to Either a string accepted by GDAL, a valid integer
#'   EPSG value, an object of class crs, or the value "raster"
#' @param coords data.frame-like object with coordinates in columns "x"
#'   and "y"
#' @param model object of the class \code{spannr_model}
#' @param add Add column coordinates to the input data.frame \code{coords}?
#'
#' @return Data.frame with converted two-dimensional coordinates
#'
#' @export
convert <- function(from, to, x = NULL, y = NULL, coords = NULL, model = NULL, add = FALSE) {
  if ((is.null(x) | is.null(y)) & is.null(coords))
    stop("Coordinates for conversion are missing", call. = FALSE)

  if ((from == "raster" | to == "raster") & is.null(model))
    stop("Model object needs to be specified for conversion of raster coordinates", call. = FALSE)

  if (add & is.null(coords))
    stop("Converted coordinates can only be added to a provided data.frame", call. = FALSE)

  if (!is.null(coords) & !all(c("x", "y") %in% colnames(coords)))
    stop("Columns 'x' and 'y' must be present in the input data.frame", call. = FALSE)

  if (!is.null(model)) {
    # dimension of the map in the projected CRS units
    bbox <- sf::st_bbox(model$map)
    map_dim <- c(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
  
    # dimension of the rasterized map in pixel units
    # (x/y dimensions of PNGs are reversed)
    raster_dim <- dim(png::readPNG(model$maps$path[1]))[2:1]
  }

  if (to == "map") to <- sf::st_crs(model$map)
  if (from == "map") from <- sf::st_crs(model$map)

  if (is.null(coords))
    df <- data.frame(x = x, y = y)
  else
    df <- coords[, c("x", "y")]

  if (from == "raster") {
    # convert pixel coordinates to na sf object in world-based coordinates
    df$x <- bbox["xmin"] + map_dim[1] * df$x / raster_dim[1]
    df$y <- bbox["ymin"] + map_dim[2] * df$y / raster_dim[2]
    point <- sf::st_as_sf(df, coords = c("x", "y"), crs = sf::st_crs(model$map))
  } else {
    # ... otherwise create a formal sf point object from the
    # coordinates already given
    point <- sf::st_as_sf(df, coords = c("x", "y"), crs = from)
  }

  if (to == "raster") {
    point_coords <- sf::st_transform(point, crs = sf::st_crs(model$map)) %>%
      sf::st_coordinates()
    newx <- abs((point_coords[, "X"] - bbox["xmin"])) / map_dim[1] * raster_dim[1]
    newy <- abs((point_coords[, "Y"] - bbox["ymin"])) / map_dim[2] * raster_dim[2]
    new_point <- data.frame(newx = round(as.vector(newx)), newy = round(as.vector(newy)))
  } else {
    new_point <- sf::st_transform(point, crs = to) %>% sf::st_coordinates()
    colnames(new_point) <- c("newx", "newy")
  }

  if (nrow(new_point) == 1)
    return(as.vector(unlist(new_point)))

  if (add) new_point <- cbind(coords, new_point)

  new_point
}
