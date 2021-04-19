#' Plot spatio-temporal population distributions
#'
#' If only geographic regions are given, they are colored. If both
#' them and populations are given, only populations are specified.
#'
#' @param ... Population/geographic region objects of the 'spammr'
#'   class
#' @param pop_facets Plot populations in individual panels?
#' @param time_facets Plot time snapshots in individual panels?
#' @param intersect intersect the population boundaries against landscape
#'   and other geographic boundaries?
#' @param geo_graticules Plot axies with lon/lat graticules?
#' @param title Plot title
#' @param nrow,ncol Number of columns or rows in the facet plot
#'
#' @export
#'
#' @import ggplot2
plot.spammr <- function(..., pop_facets = TRUE, time_facets = FALSE,
                        intersect = TRUE, geo_graticules = TRUE,
                        title = NULL, nrow = NULL, ncol = NULL) {
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
      if (intersect)
        intersect_features(i)
      else
        i
    }
  }))
  # replace the Inf split time in ancestral populations
  if (any(pops$time == Inf)) pops[pops$time == Inf, ]$time = NA

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
        geom_sf_label(data = regions, aes(label = region, color = region)) +
        guides(color = FALSE, fill = FALSE) +
        theme(axis.title = element_blank())
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

    if (pop_facets)
      pop_ids <- as.list(unique(pops$pop))
    else
      pop_ids <- list(unique(pops$pop))

    if (time_facets)
      time_facet <- facet_wrap(~ time)
    else
      time_facet <- NULL

    rows <- lapply(pop_ids, function(id) {
      p_map +
        geom_sf(data = pops[pops$pop %in% id, ],
                aes(fill = pop, alpha = -time), color = NA) +
        geom_sf(data = pops[pops$pop %in% id, ],
                fill = NA, color = "black", size = 0.1) +
        scale_fill_discrete(drop = FALSE) +
        scale_alpha(range = c(1, 0.1)) +
        ggtitle(sprintf("population: %s", id)) +
        guides(fill = FALSE, alpha = guide_legend("time")) +
        time_facet
    })

    if (length(rows) == 1) {
      p_map <- rows[[1]] +
        guides(fill = guide_legend("population")) +
        theme(plot.title = element_blank())
    } else
      p_map <- patchwork::wrap_plots(rows, ncol = ncol, nrow = nrow)
  }

  if (!is.null(title))
    p_map <- p_map + ggtitle(title)

  p_map + coord_sf(
    crs = sf::st_crs(world),
    datum = graticule_crs,
    expand = 0
  )
}


#' Print a spammr object
#'
#' @param x Object of a class spammr
#' @param sf Print the low-level 'sf' object information?
#' @param full Print all snapshots? (too many can be bothersome and require
#'   extensive scrolling)
#'
#' @export
print.spammr <- function(x, sf = FALSE, full = FALSE) {
  # name of the original argument before evaluation
  if (sf) {
    sf:::print.sf(x)
  } else {
    if (any(grepl("spammr_pop", class(x))))
      type <- "population"
    else if (any(grepl("spammr_region", class(x))))
      type <- "region"
    else if (any(grepl("spammr_world", class(x))))
      type <- "world"
    else
      type <- "model"

    header <- sprintf("spammr '%s' object", type)
    sep <- paste(rep("-", nchar(header)), collapse = "")

    cat(header, "\n")
    cat(sep, "\n")

    if (type == "population") {
      cat("name:", unique(x$pop), "\n")
      parent <- attr(x, "parent")
      cat("split from: ")
      if (is.character(parent) && parent == "ancestor")
        cat("[this is an ancestral population]\n")
      else {
        cat(parent$pop, "\n")
        cat("split time:", x$time[1], "\n")
      }
      cat("removed at: ")
      if (attr(x, "remove") == -1)
        cat("[will not be removed]\n")
      else
        cat((attr(x, "remove")), "\n")

      # pretty print the raw sf data as a simplified table
      cat("snapshots:\n")
      snapshots_df <- as.data.frame(x, stringsAsFactors = FALSE)
      snapshots_df$`#` <- 1:nrow(snapshots_df)
      # determine which maps over time are new and which are re-used from the
      # previous time point (we do this because the raw spatial geometry
      # representation is hard to read and not useful for seeing what changes
      # when)
      runs <- rle(sapply(x$geometry, tracemem))
      snapshots_df$map <- c("new", rep("same", nrow(snapshots_df) - 1))
      if (length(runs$lengths) > 1)
        snapshots_df$map[cumsum(runs$lengths)] <- "new"

      snapshots_df <- snapshots_df[, c("#", "time", "N", "map")]
      if (nrow(snapshots_df) > 15 & !full) {
        print(head(snapshots_df, 5), row.names = FALSE)
        cat("         ...\n")
        print(tail(snapshots_df, 5), row.names = FALSE)
      } else
        print(snapshots_df, row.names = FALSE)
      cat("\n")
    }

    if (type %in% c("world", "region", "population"))
      cat(paste("Coordinate Reference System: EPSG"), sf::st_crs(x)$epsg, "\n")
    else if (type == "model") {
      cat("populations:", paste0(x$splits$pop, collapse = ", "), "\n")
      cat("admixture events: ")
      if (!is.null(x$admixtures))
        cat(nrow(x$admixtures), "\n")
      else
        cat("[no admixture]\n")
      cat("generation time:", x$gen_time, "\n")
      cat("number of spatial maps:", nrow(x$maps), "\n")
      cat("resolution:", x$resolution, "km per pixel\n\n")
      cat("configuration files in:", x$config$directory, "\n\n")
      cat(
"For detailed model specification see `$splits`, `$admixtures`, `$maps`,
or `$populations` components of the model object, or the configuration
files in the model directory.\n")
    } else {
      stop("Unknown object type", call. = FALSE)
    }
  }
}


#' Set spammr classes (or fix their priorities if already present)
set_class <- function(x, type) {
  other_classes <- class(x) %>% .[!grepl("^spammr", .)]
  c("spammr", paste0("spammr_", type), other_classes)
}
