#' Check whether given population region has not yet been rendered
#'
#' @param pop Spatial population object of the 'spammr_pop' class
check_not_rendered <- function(pop) {
  if (!is.null(attr(pop, "rendered")))
    stop("An already rendered population range object was provided.
Please provide a range object before it was rendered against a map.",
call. = FALSE)
}


#' Set spammr classes (or fix their priorities if already present)
#'
#' @param x Object of the 'spammr' class
#' @param type Character scalar with the 'spammr' subtype name
set_class <- function(x, type) {
  other_classes <- class(x) %>% .[!grepl("^spammr", .)]
  c("spammr", paste0("spammr_", type), other_classes)
}


#' Define a population range
#'
#' @param name Name of the population
#' @param time Time of the population appearance
#' @param world Object of the type 'sf' which defines the world
#' @param center Vector of two elements defining a center of a circular range
#' @param radius Scalar defining a radius of a range in kilometers
#' @param coords List of vector pairs, defining corners of the range
#'
#' @export
population <- function(name, time, world, center = NULL, radius = NULL, coords = NULL,
                       region = NULL) {
  # define the population range as a simple geometry object
  # and bind it with the annotation info into an sf object
  if (!is.null(region) & is.null(center)) {
    range <- sf::st_sfc(sf::st_geometry(region))
  } else {
    range <- spatial_range(world, center, radius, coords)
  }
  pop_range <- sf::st_sf(
    data.frame(pop = name, time = time),
    geometry = range
  )

  sf::st_agr(pop_range) <- "constant"

  # keep the world as an internal attribute
  attr(pop_range, "world") <- world
  # optionally, keep a restricted population region
  if (!is.null(region) & !is.null(center))
    attr(pop_range, "region") <- region

  class(pop_range) <- set_class(pop_range, "pop")
  pop_range
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
#'
#' @param coords List of vectors (pairs of longitude and latitude values)
#' @param world Object of the type 'sf' which defines the world
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


#' Define a world map for all spatial operations
#'
#' Download Natural Earth land area map data, zoom on a given window
#' determined by longitude and latitude coordinates and transform to a
#' specified projection
#'
#' @param lon Numeric vector with minimum and maximum longitude
#' @param lat Numeric vector with minimum and maximum latitude
#' @param crs Coordinate Reference System to use for all spatial
#'   operations (default is WGS-84 or EPSG:4326 CRS)
#'
#' @export
world_map <- function(lon, lat, crs = "EPSG:4326") {
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
  zoom_bounds <- define_zoom(lon, lat, "EPSG:4326")
  zoom_transf <- sf::st_transform(zoom_bounds, crs)

  ## crop the map to the boundary coordinates
  world_zoom <- sf::st_crop(world_transf, zoom_transf)

  sf::st_agr(world_zoom) <- "constant"

  class(world_zoom) <- set_class(world_zoom, "world")

  world_zoom
}


#' Take a list of all population regions and intersect them with the
#' set of underlying world map
#'
#' @param pop Spatial population object of the 'spammr_pop' class
#'
#' @export
render <- function(pop) {
  world <- attr(pop, "world")
  region <- attr(pop, "region")

  # restrict the population range to the landscape features
  rendered <- sf::st_intersection(pop, sf::st_geometry(world))

  # restrict further to the geographic boundary, if given
  if (!is.null(region)) {
    sf::st_agr(rendered) <- "constant"
    rendered <- sf::st_intersection(rendered, sf::st_geometry(region))
  }

  sf::st_agr(rendered) <- "constant"

  # add a small tag signifying that the ranges have been processed
  # and intersected over the map of the world
  attr(rendered, "rendered") <- TRUE
  # add back the world attribute
  attr(rendered, "world") <- world

  class(rendered) <- set_class(rendered, "pop")
  rendered
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
  check_not_rendered(pop)

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
  # optionally, add a migration boundary
  attr(inter_regions, "region") <- region

  class(inter_regions) <- set_class(inter_regions, "region")
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
  check_not_rendered(pop)

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
      data.frame(pop = region_start$pop, time = traj_diffs[i, "time"]),
      geometry = shifted_region,
      crs = sf::st_crs(inter_regions[[i]])
    )
  }

  inter_regions <- rbind(pop[-nrow(pop), ], do.call(rbind, inter_regions))
  sf::st_agr(inter_regions) <- "constant"

  # keep the world as an internal attribute
  attr(inter_regions, "world") <- attr(pop, "world")

  class(inter_regions) <- set_class(inter_regions, "pop")

  inter_regions
}


#' Plot spatio-temporal population distributions
#'
#' Plot the spatio-temporal distributions of populations and
#' geographic regions across a map.
#'
#' If only geographic regions are given, they are colored. If both
#' them and populations are given, only populations are specified.
#'
#' @param ... Population/geographic region objects of the 'spammr'
#'   class
#' @param facets Plot populations in individual panels?
#' @param rendering Render the population boundaries against landscape
#'   and other geographic boundaries?
#' @param geo_graticules Plot axies with lon/lat graticules?
#'
#' @export
#'
#' @import ggplot2
plot.spammr <- function(..., facets = TRUE, rendering = TRUE, geo_graticules = TRUE, title = NULL) {
  args <- list(...)
  # only the world object being plotted?
  if (length(args) == 1 & inherits(args[[1]], "spammr_world"))
    world <- args[[1]]
  else {
    # extract the world component underlying each population object
    # and make sure they are all the same with no conflicts
    world <- unique(lapply(args, function(i) attr(i, "world")))
    if (length(world) != 1) {
      stop("Population objects do not share the same world component", call. = F)
    } else {
      world <- world[[1]]
    }
  }

  regions <- do.call(rbind, lapply(list(...), function(i) if (!is.null(i$region)) i))
  pops <- do.call(rbind, lapply(list(...), function(i) {
    if (!is.null(i$pop)) {
      if (rendering)
        render(i)
      else
        i
    }
  }))

  # plot the world map
  p_map <-  ggplot() +
    geom_sf(data = world, fill = NA, color = "black") +
    theme_bw()

  # plot geographic region boundaries, if present
  if (!is.null(regions)) {
    # plot in colors only when no populations are present
    if (is.null(pops)) {
      p_map <- p_map +
        geom_sf(data = regions, aes(fill = region), linetype = 2, alpha = 0.5) +
        geom_sf_label(data = regions, aes(label = region, color = region))
    } else {
      p_map <- p_map +
        geom_sf(data = regions, fill = "lightgray", linetype = 2, alpha = 0.5) +
        geom_sf_label(data = regions, aes(label = region))
    }
  }

  if (geo_graticules)
    graticule_crs <- "EPSG:4326"
  else
    graticule_crs <- sf::st_crs(world)

  # plot population ranges, if present
  if (!is.null(pops)) {
    pops$pop <- factor(pops$pop)

    if (facets)
      pop_ids <- as.list(unique(pops$pop))
    else
      pop_ids <- list(unique(pops$pop))

    rows <- lapply(pop_ids, function(id) {
      p_map +
        geom_sf(data = pops[pops$pop %in% id, ],
                aes(fill = pop, alpha = -time), color = NA) +
        geom_sf(data = pops[pops$pop %in% id, ],
                fill = NA, color = "black", size = 0.1) +
        scale_fill_discrete(drop = FALSE) +
        scale_alpha(range = c(1, 0.1)) +
        ggtitle(sprintf("population: %s", id)) +
        guides(fill = FALSE, alpha = guide_legend("time"))
    })

    if (length(rows) == 1) {
      p_map <- rows[[1]] +
        guides(fill = guide_legend("population"))
    } else
      p_map <- patchwork::wrap_plots(rows)
  }

  if (!is.null(title))
    p_map <- p_map + ggtitle(title)

  p_map + coord_sf(
    crs = sf::st_crs(world),
    datum = graticule_crs,
    expand = 0
  )
}


#' Render the population boundary to a black-and-white rasterized
#' spatial map
#'
#' @param ... Spatial population objects of the 'spammr_pop' class
#' @param outdir Output directory to save all spatial maps
#' @param rendering Render the population boundaries against landscape
#'   and other geographic boundaries?
#'
#' @import ggplot2
#' @export
rasterize <- function(..., outdir = NULL, rendering = TRUE) {
  pops <- list(...)
  raster_list <- lapply(pops, function(pop) {
    times <- unique(pop$time)
    snapshots <- lapply(times, function(t) {
      snapshot <- pop[pop$time == t, ]
      class(snapshot) <- set_class(snapshot, "pop")

      # render the population if needed
      if (is.null(attr(pop, "rendered")) & rendering)
        snapshot <- render(snapshot)

      bbox <- sf::st_bbox(attr(snapshot, "world"))
      ggplot() +
        geom_sf(data = snapshot, fill = "white", color = NA) +
        coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)], expand = FALSE) +
        theme_void() +
        theme(plot.background = element_rect(fill = "black"))
    })
    names(snapshots) <- paste(pop$pop, times, sep = "_")
    snapshots
  })

  # flatten the list of ggplot objects
  rasters <- do.call(c, raster_list)

  if (is.null(outdir))
    return(rasters)
  else {
    for (i in names(rasters)) {
      if (!dir.exists(outdir)) dir.create(outdir, showWarnings = FALSE)
      path <- file.path(outdir, paste0(i, ".png"))
      ggsave(path, rasters[[i]])
    }
  }
}
