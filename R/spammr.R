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
    pops <- pops[order(pops$time, decreasing = TRUE), ]
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
        guides(fill = guide_legend("population"))
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
#'
#' @export
print.spammr <- function(x, sf = FALSE) {
  if (sf) {
    sf:::print.sf(x)
  } else {
    if (grepl("spammr_pop", class(x)[2]))
      type <- "population"
    else if (grepl("spammr_region", class(x)[2]))
      type <- "region"
    else
      type <- "world"

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
      snapshots_df <- as.data.frame(x)
      snapshots_df$`#` <- 1:nrow(snapshots_df)
      # determine which maps over time are new and which are re-used from the
      # previous time point (we do this because the raw spatial geometry
      # representation is hard to read and not useful for seeing what changes
      # when)
      runs <- rle(sapply(x$geometry, tracemem))
      snapshots_df$map <- c("new", rep("same", nrow(snapshots_df) - 1))
      if (length(runs$lengths) > 1)
        snapshots_df$map[cumsum(runs$lengths)] <- "new"

      print(snapshots_df[, c("#", "time", "Ne", "map")], row.names = FALSE)
      cat("\n")
    }

    # extract projection type and name using the internal sf plumbing
    crs_info <- sf:::CPL_crs_parameters(sf::st_crs(x))
    if (crs_info$IsGeographic)
      cat(paste0("geographic CRS: ", crs_info$Name, "\n"))
    else
      cat(paste0("projected CRS: ", crs_info$Name, "\n"))
  }
}


#' Set spammr classes (or fix their priorities if already present)
set_class <- function(x, type) {
  other_classes <- class(x) %>% .[!grepl("^spammr", .)]
  c("spammr", paste0("spammr_", type), other_classes)
}
