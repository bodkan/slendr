#' Compile the spatial maps and split/admixture tables
#'
#' @param ... Population object of the 'spammr_pop' class
#' @param outdir Directory to which to save the entire model
#' @param dry_run Return the compiled model components without saving them to
#'   disk?
#' @param overwrite Overwrite the contents of the output directory (in case it
#'   already exists)?
#'
#' @export
compile <- function(..., outdir = NULL, dry_run = FALSE, overwrite = FALSE) {
  if (is.null(outdir) & !dry_run)
    stop("Output directory for the model specification files missing",
         call. = FALSE)

  pops <- list(...)

  # iterate over all spammr population objects and extract
  # information about population split hierarchy and split times
  # into a data frame
  splits_table <- lapply(pops, function(p) {
    parent <- attr(p, "parent")
   if (is.character(parent) && parent == "ancestor") {
     parent_name <- parent
   } else {
     parent_name <- unique(attr(p, "parent")$pop)
   }

    data.frame(
      pop = unique(p$pop),
      parent = parent_name,
      time = p$time[1],
      Ne = unique(p$Ne)
    )
  }) %>% do.call(rbind, .)

  # order populations by split time and assign a numeric identifier to each
  splits_table <- splits_table[
    order(splits_table$time, decreasing = TRUE, na.last = FALSE), ]
  splits_table$pop_id <- 1:nrow(splits_table) - 1
  splits_table$parent_id <- lapply(
    splits_table$parent,
    function(i) splits_table[splits_table$pop == i, ]$pop_id
  ) %>% unlist() %>% c(NA, .)
  # take care of Inf/NA values for downstream SLiM processing
  splits_table$time <- ifelse(splits_table$time == Inf, -1, splits_table$time)
  splits_table$parent_id <- ifelse(is.na(splits_table$parent_id), -1, splits_table$parent_id)

  # generate spatial bit maps
  maps <- render(...)

  # convert list of rasters into data frame, adding the spatial
  # maps themselves as a list column
  maps_table <- lapply(maps, function(m) {
    as.data.frame(m[c("pop", "time")])
  }) %>%
    do.call(rbind, .)
  # add column with a numeric population identifier (used later by SLiM)
  maps_table$pop_id <- unlist(lapply(
    maps_table$pop,
    function(i) splits_table[splits_table$pop == i, ]$pop_id
  ))
  maps_table$map <- I(lapply(maps, function(m) m$map))


  # return the finalized model specification without saving it to disk
  if (dry_run) {
    return(list(
      splits = splits_table,
      maps = maps_table
    ))
  }

  # prepare the model output directory
  if (dir.exists(outdir)) {
    if (!overwrite)
      stop("Output directory already exists - either delete it or set 'overwrite = TRUE'",
           call. = FALSE)
    else
      unlink(outdir, recursive = TRUE, force = TRUE)

  }
  dir.create(outdir)

  # save the rasterized spatial maps
  for (i in seq_len(nrow(maps_table))) {
    map_row <- maps_table[i, ]
    # the first spatial map has necessarily a nonsensical time stamp,
    # so let's take care of that first
    if (map_row$time == Inf) {
      time <- "ancestor"
      maps_table[i, "time"] <- -1
    } else {
      time <- map_row$time
    }
    filename <- sprintf("map_%d_%s_%s.png", i, map_row$pop, time)
    path <- file.path(outdir, filename)
    ggsave(path, map_row$map[[1]])
    maps_table[i, "file"] <- path
  }

  # save the table with spatial map paths
  write.table(
    maps_table[, c("pop", "pop_id", "time", "file")],
    file.path(outdir, "spatial_maps.tsv"),
    sep = "\t", quote = FALSE, row.names = FALSE
  )

  # save the population splits table
  write.table(
    splits_table,
    file.path(outdir, "population_splits.tsv"),
    sep = "\t", quote = FALSE, row.names = FALSE
  )
}


#' Get the centroid of the first population range
get_centroids <- function(pop) {
  pop[1, ] %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.vector
}


#' Render population boundaries to black-and-white spatial maps
#'
#' @param ... Spatial population objects of the 'spammr_pop' class
#'
#' @import ggplot2
render <- function(...) {
  pops <- list(...)
  raster_list <- lapply(pops, function(pop) {
    # iterate over temporal maps for the current population
    snapshots <- lapply(unique(pop$time), function(t) {
      snapshot <- pop[pop$time == t, ]
      class(snapshot) <- set_class(snapshot, "pop")

      # render the population if needed
      if (is.null(attr(pop, "intersected")))
        snapshot <- intersect_features(snapshot)

      # plot the spatial map, using the 'world' context as a bounding box
      bbox <- sf::st_bbox(attr(snapshot, "world"))
      p_map <- ggplot() +
        geom_sf(data = snapshot, fill = "white", color = NA) +
        coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)], expand = FALSE) +
        theme_void() +
        theme(plot.background = element_rect(fill = "black"))

      # return the rendered spatial map with the population name and the
      # appropriate time stamp (unique-ing because intersecting splits
      # the spatial object into multiple disjoint features)
      list(
        pop = unique(snapshot$pop),
        time = unique(snapshot$time),
        map = p_map
      )
    })
    snapshots
  })

  # flatten the list of ggplot objects
  rasters <- do.call(c, raster_list)

  rasters
}


#' Open the SLiM backend script in the SLiM gui
#'
#' @export
run_slimgui <- function() {
  # backend_script <- system.file("backend.slim", package = "spammr")
  backend_script <- "~/projects/spammr/inst/extdata/backend.slim"
  system(sprintf("open -a SLiMgui %s", backend_script))
}

