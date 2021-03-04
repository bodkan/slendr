#' Render population boundaries to black-and-white spatial maps
#'
#' @param ... Spatial population objects of the 'spammr_pop' class
#' @param outdir Output directory to save all spatial maps
#' @param rendering Render the population boundaries against landscape
#'   and other geographic boundaries?
#'
#' @import ggplot2
render <- function(..., outdir = NULL, intersect = TRUE) {
  pops <- list(...)
  raster_list <- lapply(pops, function(pop) {
    times <- unique(pop$time)
    snapshots <- lapply(times, function(t) {
      snapshot <- pop[pop$time == t, ]
      class(snapshot) <- set_class(snapshot, "pop")

      # render the population if needed
      if (is.null(attr(pop, "intersected")) & intersect)
        snapshot <- intersect_features(snapshot)

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
