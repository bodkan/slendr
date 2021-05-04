#' Plot spatio-temporal population distributions
#'
#' If only geographic regions are given, they are colored. If both
#' them and populations are given, only populations are specified.
#'
#' @param ... Population/geographic region objects of the 'spannr'
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
plot.spannr <- function(..., pop_facets = TRUE, time_facets = FALSE,
                        intersect = TRUE, geo_graticules = TRUE,
                        title = NULL, nrow = NULL, ncol = NULL) {
  args <- list(...)
  # is only the world object being plotted?
  if (length(args) == 1 & inherits(args[[1]], "spannr_map"))
    map <- args[[1]]
  else {
    # extract the map component underlying each population object
    # and make sure they are all the same with no conflicts
    maps <- unique(lapply(args, function(i) attr(i, "map")))
    if (length(maps) != 1) {
      stop("Objects do not share the same map component", call. = F)
    }
    map <- maps[[1]]
  }

  regions <- do.call(rbind, lapply(list(...), function(i) if (!is.null(i$region)) i))
  pops <- do.call(rbind, lapply(list(...), function(i) {
    if (!is.null(i$pop)) {
      if (intersect & nrow(map))
        intersect_features(i)
      else
        i
    }
  }))
  # replace the Inf split time in ancestral populations (invisible otherwise)
  if (any(pops$time == Inf)) pops[pops$time == Inf, ]$time = NA

  p_map <-  ggplot() + theme_bw()
  
  # plot the world map if a real geographic map was specified
  if (nrow(map))
    p_map <- p_map + geom_sf(data = map, fill = "lightgray", color = NA)

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

  if (geo_graticules & has_crs(map))
    graticule_crs <- "EPSG:4326"
  else
    graticule_crs <- sf::st_crs(map)

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

  if (has_crs(map))
    return(p_map & coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0))
  else
    return(p_map & coord_sf(
      xlim = attr(map, "xrange"),
      ylim = attr(map, "yrange"),
      expand = 0
    ))
}


#' Print a spannr object
#'
#' @param x Object of a class spannr
#' @param sf Print the low-level 'sf' object information?
#' @param full Print all snapshots? (too many can be bothersome and require
#'   extensive scrolling)
#'
#' @export
print.spannr <- function(x, sf = FALSE, full = FALSE) {
  if (sf) {
    sf:::print.sf(x)
  } else {
    if (any(grepl("spannr_pop", class(x))))
      type <- "population"
    else if (any(grepl("spannr_region", class(x))))
      type <- "region"
    else if (any(grepl("spannr_map", class(x))))
      type <- "map"
    else if (any(grepl("spannr_model", class(x))))
      type <- "model"
    else
      stop("Unknown object")

    header <- sprintf("spannr '%s' object", type)
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

    if (type %in% c("map", "region", "population")) {
      crs <- sf::st_crs(x)$epsg
      if (is.na(crs)) {
        cat("abstract spatial landscape ")
        if (nrow(x))
          cat("with custom features\n")
        else
          cat("with no features\n")
        units <- ""
      } else {
        crs <- paste("EPSG", crs)
        cat("internal coordinate reference system:", crs, "\n")
        units <- " (in degrees longitude and latitude)"
      }
      xrange <- attr(x, "xrange")
      yrange <- attr(x, "yrange")
      cat(sprintf("spatial limits%s:\n  - vertical %d ... %d\n  - horizontal %d ... %d\n",
                  units, xrange[1], xrange[2], yrange[1], yrange[2]))
    } else if (type == "model") {
      cat("populations:", paste0(x$splits$pop, collapse = ", "), "\n")
      cat("admixture events: ")
      if (!is.null(x$admixtures))
        cat(nrow(x$admixtures), "\n")
      else
        cat("[no admixture]\n")
      cat("generation time:", x$gen_time, "\n")
      cat("number of spatial maps:", nrow(x$maps), "\n")
      cat("resolution:", x$resolution, "km per pixel\n\n")
      cat("configuration files in:", normalizePath(x$config$directory), "\n\n")
      cat(
"For detailed model specification see `$splits`, `$admixtures`, `$maps`,
or `$populations` components of the model object, or the configuration
files in the model directory.\n")
    } else {
      stop("Unknown object type", call. = FALSE)
    }
  }
}


#' Set spannr classes (or fix their priorities if already present)
set_class <- function(x, type) {
  other_classes <- class(x) %>% .[!grepl("^spannr", .)]
  c("spannr", paste0("spannr_", type), other_classes)
}
