#' Compile the spatial maps and split/admixture tables
#'
#' @param ... Population object of the 'spammr_pop' class
#' @param dry_run Return the compiled model components without saving them to
#'   disk?
#'
#' @export
compile <- function(..., dry_run = T) {
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

  # order populations by split time and assign a numberic identifier to each
  splits_table <- splits_table[order(splits_table$time, decreasing = TRUE), ]
  splits_table$pop_id <- 1:nrow(splits_table) - 1
  splits_table$parent_id <- lapply(
    splits_table$parent,
    function(i) splits_table[splits_table$pop == i, ]$pop_id
  ) %>% unlist() %>% c(-1, .)

  # generate spatial bit maps
  maps <- render(..., intersect = T)

  # convert list of rasters into data frame, adding the spatial
  # maps themselves as a list column
  maps_table <- lapply(maps, function(m) {
    as.data.frame(m[c("pop_name", "time")])
  }) %>%
    do.call(rbind, .)
  maps_table$map <- I(lapply(maps, function(m) m$map))

  if (dry_run) {
    return(list(
      splits = splits_table,
      maps = maps_table
    ))
  }
}


#' Render population boundaries to black-and-white spatial maps
#'
#' @param ... Spatial population objects of the 'spammr_pop' class
#' @param outdir Output directory to save all spatial maps
#' @param rendering Render the population boundaries against landscape
#'   and other geographic boundaries?
#'
#' @import ggplot2
render <- function(..., intersect = TRUE) {
  pops <- list(...)
  raster_list <- lapply(pops, function(pop) {
    # iterate over temporal maps for the current population
    snapshots <- lapply(unique(pop$time), function(t) {
      snapshot <- pop[pop$time == t, ]
      class(snapshot) <- set_class(snapshot, "pop")

      # render the population if needed
      if (is.null(attr(pop, "intersected")) & intersect)
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
        pop_name = unique(snapshot$pop),
        time = unique(snapshot$time),
        map = p_map
      )
    })
    snapshots
  })

  # flatten the list of ggplot objects
  rasters <- do.call(c, raster_list)

  rasters
  # if (is.null(outdir))
  #   return(rasters)
  # else {
  #   for (i in names(rasters)) {
  #     if (!dir.exists(outdir)) dir.create(outdir, showWarnings = FALSE)
  #     path <- file.path(outdir, paste0(i, ".png"))
  #     ggsave(path, rasters[[i]])
  #   }
  # }
}
