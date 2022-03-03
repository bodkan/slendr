#' Plot \code{slendr} geographic features on a map
#'
#' Plots objects of the three \code{slendr} spatial classes (\code{slendr_map},
#' \code{slendr_region}, and \code{slendr_pop}).
#'
#' @param ... Objects of classes \code{slendr_map}, \code{slendr_region}, or
#'   \code{slendr_pop}
#' @param time Plot a concrete time point
#' @param geneflows Indicate geneflow events with an arrow
#' @param graticules Plot graticules in the original Coordinate Reference System
#'   (such as longitude-latitude), or in the internal CRS (such as meters)?
#' @param intersect Intersect the population boundaries against landscape and
#'   other geographic boundaries (default TRUE)?
#' @param show_map Show the underlying world map
#' @param title Title of the plot
#' @param interpolated_maps Interpolated spatial boundaries for all populations
#'   in all time points (this is only used for plotting using the \code{explore}
#'   shiny app)
#'
#' @export
#'
#' @import ggplot2
plot_map <- function(..., time = NULL, geneflows = FALSE,
                     graticules = "original",
                     intersect = TRUE, show_map = TRUE,
                     title = NULL, interpolated_maps = NULL) {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

  if (is.null(show_map)) show_map <- FALSE

  args <- list(...)

  if (!all(sapply(args, inherits, "slendr")))
    stop("Only objects of the class 'slendr' can be visualized using plot.slendr", call. = FALSE)

  classes <- grep("slendr_", unique(unlist(sapply(args, class))), value = TRUE)
  if (length(classes) > 1 & "slendr_model" %in% classes)
    stop("If a 'slendr_model' object is to be plotted, it must be a single argument", call. = FALSE)
  if (length(classes) > 1 & "slendr_map" %in% classes)
    stop("If a 'slendr_map' object is to be plotted, it must be a single argument", call. = FALSE)
  if (length(intersect(c("slendr_region", "slendr_pop"), classes)) > 1)
    stop("'slendr_region' and 'slendr_pops' object cannot be plotted at once", call. = FALSE)

  if (geneflows & (is.null(time) | !inherits(args[[1]], "slendr_model")))
    stop("Migrations can be visualized only when a time point and a 'slendr_model'
objects are specified", call. = FALSE)

  # a single model object was provided
  if (length(args) == 1 & inherits(args[[1]], "slendr_model")) {
    model <- args[[1]]
    pops <- model$populations
    map <- model$world
    regions <- list()
  } else if (all(classes == "slendr_map")) {
    map <- args[[1]]
    regions <- pops <- list()
  } else if (all(classes == "slendr_ts")) {
    map <- args[[1]]
    regions <- pops <- list()
  } else {
    # extract the map component underlying each population object
    # and make sure they are all the same with no conflicts
    maps <- unique(lapply(args, function(i) attr(i, "map")))
    if (length(maps) != 1) {
      stop("Objects do not share the same map component", call. = FALSE)
    }
    map <- maps[[1]]

    regions <- lapply(args, function(i) if (!is.null(i$region)) i) %>% Filter(Negate(is.null), .)
    pops <- lapply(args, function(i) if (!is.null(i$pop)) i) %>% Filter(Negate(is.null), .)

    if (!all(sapply(pops, has_map)))
      stop("Cannot plot spatial maps for non-spatial models", call. = FALSE)
  }

  if (graticules == "original" & has_crs(map)) {
    graticule_crs <- "EPSG:4326"
    xlab <- "longitude"
    ylab <- "latitude"
  } else {
    graticule_crs <- sf::st_crs(map)
    xlab <- ylab <- NULL
  }

  if (has_crs(map)) {
    bbox <- sf::st_bbox(map)
    p_coord <- coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0,
                        xlim = c(bbox["xmin"], bbox["xmax"]),
                        ylim = c(bbox["ymin"], bbox["ymax"]))
  } else {
    p_coord <- coord_sf(
      xlim = attr(map, "xrange"),
      ylim = attr(map, "yrange"),
      expand = 0
    )
  }

  p <- ggplot()

  if (!is.null(map) && nrow(map) && show_map)
    p <- p + geom_sf(data = map, aes(frame = NULL), fill = "lightgray", color = NA)

  if (length(pops)) {
    # extract population split times and order population names in the order of
    # their appearance in the simulation
    direction <- setdiff(unique(sapply(pops, time_direction)), "unknown")
    split_times <- purrr::map_int(pops, function(pop) {
      attr(pop, "history")[[1]]$time
    })
    if (direction == "backward") {
      split_times <- sort(split_times, decreasing = TRUE)
    } else {
      split_times <- sort(split_times)
    }
    pop_names <- names(split_times)

    # if the user specified a time point, "interpolate" all maps at that
    # time and return just those that match that time point (unless this
    # was already pre-computed)
    if (!is.null(time)) {
      if (is.null(interpolated_maps))
        interpolated_maps <- fill_maps(pops, time)

      # get all time points defined by the user
      all_times <- sort(unique(unlist(lapply(pops, `[[`, "time"))))

      # get split and removal times of all specified populations
      split_times <- sapply(pops, function(p) { attr(p, "history")[[1]]$time })
      removal_times <- sapply(pops, attr, "remove")

      previous_time <- min(all_times[all_times >= time])
      # get only those populations already/still present at the
      # specified time...
      present_pops <- interpolated_maps[split_times >= time & removal_times <= time]
      # ... and extract their spatial maps
      pop_maps <- lapply(present_pops, function(pop) {
        snapshot <- pop[pop$time == previous_time, ]
        attributes(snapshot) <- attributes(pop)
        snapshot
      })
    } else {
      pop_maps <- pops
    }

    if (intersect) pop_maps <- lapply(pop_maps, intersect_features)

    pop_maps <- do.call(rbind, pop_maps)
    pop_maps$pop <- factor(pop_maps$pop, levels = pop_names)

    if (length(unique(pop_maps$time)) > 1) {
      # build a base map with geographic features
      p <- p +
        geom_sf(data = pop_maps, aes(fill = pop, alpha = time), color = NA) +
        geom_sf(data = pop_maps, fill = NA, color = "black", size = 0.1)
    } else {
      p <- p +
        geom_sf(data = pop_maps, aes(fill = pop), color = NA, alpha = 0.4) +
        geom_sf(data = pop_maps, fill = NA, color = "black", size = 0.1)
    }
    p <- p + scale_fill_discrete(drop = FALSE, name = "") + guides(alpha = guide_legend("time"))

    # add geneflow arrows, if requested
    if (geneflows) {
      migr_df <- get_geneflows(model, time)
      if (nrow(migr_df))
        p <- p +
          geom_point(data = migr_df, aes(x = from_x, y = from_y, color = from), size = 7) +
          geom_point(data = migr_df, aes(x = to_x, y = to_y, color = to), size = 7) +
          geom_curve(
            data = migr_df,
            aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
            arrow = arrow(length = unit(2, "mm"), type = "closed"),
            lineend = "round", size = 0.5, arrow.fill = "black"
          ) +
          scale_color_discrete(drop = FALSE) +
          guides(color = "none")
    }
  }

  if (length(regions)) {
    region_maps <- do.call(rbind, regions)
    p <- p +
      geom_sf(data = region_maps, aes(fill = region), linetype = 2, alpha = 0.5) +
      geom_sf_label(data = region_maps, aes(label = region, color = region)) +
      guides(color = "none", fill = "none")
  }

  if (!is.null(title)) p <- p + ggtitle(title)

  p + labs(x = xlab, y = ylab) +
    theme_bw() +
    p_coord
}

#' Plot demographic history encoded in a slendr model
#'
#' @param model Compiled \code{slendr_model} model object
#' @param sizes Should population size changes be visualized?
#' @param proportions Should gene flow proportions be visualized (\code{FALSE}
#'   by default to prevent cluttering and overplotting)
#' @param log Should the y-axis be plottted on a log scale? Useful for models
#'   over very long time-scales.
#'
#' @return Plot object of the class \code{ggplot}
#' @export
plot_model <- function(model, sizes = TRUE, proportions = FALSE, log = FALSE) {
  populations <- model$populations

  # extract population split times and order population names in the order of
  # their appearance in the simulation
  split_times <- purrr::map_int(populations, function(pop) {
    attr(pop, "history")[[1]]$time
  })
  if (model$direction == "backward") {
    split_times <- sort(split_times, decreasing = TRUE)
  } else {
    split_times <- sort(split_times)
  }
  pop_names <- names(split_times)

  # extract times at which each population will be removed from the simulation
  default_end <- if (model$direction == "backward") 0 else model$orig_length
  end_times <- purrr::map_int(populations, function(pop) {
    remove <- attr(pop, "remove")
    if (remove == -1)
      return(as.integer(default_end))
    else if (remove > 0)
      return(as.integer(remove))
    else
      stop("Unknown end time", call. = FALSE)
  })
  end_times <- end_times[pop_names]

  # extract the size of each population at the end of its existence
  if (sizes) {
    final_sizes <- purrr::map_int(populations, function(pop) {
      history <- rev(attr(pop, "history"))
      purrr::map(history, ~ .$N) %>%
        purrr::keep(~ !is.null(.x)) %>%
        .[[1]] %>%
        as.integer()
    })
    final_sizes <- final_sizes[pop_names]
  } else {
    default_N <- 10000
    final_sizes <- rep(default_N, length(populations))
  }

  # compute center of each "population column" along the bottom of the x-axis
  centers <- dplyr::tibble(
    pop = factor(pop_names, levels = pop_names),
    N = final_sizes,
    time = split_times,
  ) %>%
    dplyr::mutate(xmax = cumsum(N + median(N)),
                  xmin = xmax - N,
                  center = xmin + N / 2) %>%
    dplyr::select(pop, center, time)

  # iterate over each population's demographic history and compose the
  # coordinates of polygons in each epoch
  size_changes <- lapply(populations, function(pop) {
    # pop <- populations[[i]]
    name <- pop$pop[1]
    center <- centers[centers$pop == name, ]$center
    history <- attr(pop, "history") %>%
      purrr::keep(~ .$event %in% c("split", "resize")) %>%
      rev() %>%
      c(list(dplyr::tibble(pop = name, time = end_times[name], N = .[[1]]$N, event = "remove")), .)

    purrr::map(seq_len(length(history))[-length(history)], function(j) {
      current_event <- history[[j]]
      next_event <- history[[j + 1]]

      # time of the current event
      if (current_event$event == "remove")
        ys <- rep(current_event$time, 2)
      else if (current_event$event == "resize")
        ys <- rep(current_event$tresize, 2)
      else
        stop("Invalid 'current event'. This is a slendr bug! (1)", call. = FALSE)

      # time of the next event
      if (next_event$event == "split")
        ys <- c(ys, rep(next_event$time, 2))
      else if (next_event$event == "resize")
        ys <- c(ys, rep(next_event$tresize, 2))
      else
        stop("Invalid 'next event'. This is a slendr bug! (2)", call. = FALSE)

      if (!sizes) {
        current_event$prev_N <- current_event$N <- default_N
        next_event$prev_N <- next_event$N <- default_N
      }
      # population sizes
      if (current_event$event == "resize")
        xs <- c(center - current_event$prev_N / 2, center + current_event$prev_N / 2)
      else
        xs <- c(center - current_event$N / 2, center + current_event$N / 2)

      if (next_event$event == "split" || next_event$how == "step")
        xs <- c(xs, center + next_event$N / 2, center - next_event$N / 2)
      else if (next_event$event == "split" || next_event$how == "exponential")
        xs <- c(xs, center + next_event$prev_N / 2, center - next_event$prev_N / 2)
      else
        stop("Invalid 'next event'. This is a slendr bug! (4)", call. = FALSE)

      dplyr::tibble(
        pop = factor(name, levels = pop_names),
        x = xs,
        y = ifelse(ys == 0, 0.001, ys)
      )
    })
  })

  # generate a table of population split times and population sizes to be used
  # for plotting horizontal split lines
  splits <- list()
  for (pop in populations) {
    pop_name <- pop$pop[1]
    parent <- attr(pop, "parent")
    event <- attr(pop, "history")[[1]]
    if (inherits(parent, "slendr_pop")) {
      parent_name <- parent$pop[1]
      from_x <- centers[centers$pop == parent_name, ]$center
      to_x <- centers[centers$pop == pop_name, ]$center
      time_y <- event$time
      splits[[length(splits) + 1]] <- dplyr::tibble(
        pop = factor(pop_name, levels = pop_names),
        from = factor(parent_name, levels = pop_names),
        x = from_x,
        xend = to_x,
        y = time_y,
        yend = time_y
      )
    }
  }
  splits <- do.call(rbind, splits)

  # generate a table of gene flow events to be used for plotting gene flow
  # arrows
  if (!is.null(model$geneflow)) {
    geneflows <- model$geneflow %>%
      dplyr::mutate(x = purrr::map_dbl(from, ~ centers[centers$pop == .x, ]$center),
             xend = purrr::map_dbl(to, ~ centers[centers$pop == .x, ]$center),
             y = tstart_orig,
             yend = tend_orig)
  } else
    geneflows <- NULL

  if (model$direction == "forward")
    ylabel <- "time since the start"
  else
    ylabel <- "time before present"

  if (log) ylabel <- paste(ylabel, "(log scale)")

  # setup a figure outline
  p <- ggplot()  +
    scale_color_discrete(drop = FALSE) +
    scale_fill_discrete(drop = FALSE) +
    labs(y = ylabel) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(arrow = grid::arrow(
        length = unit(0.25, "cm"),
        ends = "first"
      ))
    )

  if (log) p <- p + scale_y_log10()

  if (model$direction == "forward") p <- p + scale_y_reverse()

  # add horizontal split lines
  if (!is.null(splits)) {
    p <- p + geom_segment(
      data = splits,
      aes(x = x, xend = xend, y = y, yend = yend, color = from),
      size = 2
    )
  }

  # add each each epoch resize segment
  for (lineage in size_changes) {
    for (event in lineage)
      p <- p + geom_polygon(data = event, aes(x, y, fill = pop))
  }

  # add gene flow arrows and proportion labels
  if (!is.null(geneflows)) {
    p <- p + geom_segment(data = geneflows,
                          aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = arrow(length = unit(0.2, "cm")))
    p <- p + geom_point(data = geneflows, aes(x = x, y = y))
    if (proportions)
      p <- p + geom_label(data = geneflows,
                          aes(label = sprintf("%s%%", 100 * rate),
                              x = xend - (xend - x) / 2,
                              y = yend - (yend - y) / 2), size = 3)
  }

  # labels with population names
  p <- p + geom_label(data = centers, aes(label = pop, x = center, y = time,
                                          fill = pop, fontface = "bold"))

  p
}

#' Animate the simulated population dynamics
#'
#' @param model Compiled \code{slendr_model} model object
#' @param file Path to the table of saved individual locations
#' @param steps How many frames should the animation have?
#' @param gif Path to an output GIF file (animation object returned
#'   by default)
#' @param width,height Dimensions of the animation in pixels
#'
#' @return If `gif = NULL`, return gganimate animation object. Otherwise a GIF
#'   file is saved and no value is returned.
#'
#' @import ggplot2
#' @export
animate_model <- function(model, file = file.path(model$path, "output_ind_locations.tsv.gz"),
                          steps, gif = NULL, width = 800, height = 560) {
  if (!"magick" %in% utils::installed.packages()[, 1])
    message("For rendering animated GIFs, please install the R package ",
            "magick by calling `install.packages(\"magick\")")

  if (!inherits(model$world, "slendr_map"))
    stop("Cannot animate non-spatial models", call. = FALSE)

  pop_names <- model$splits$pop

  locs <- readr::read_tsv(file, col_types = "iicidd", progress = FALSE) %>%
    dplyr::mutate(
      time = convert_slim_time(gen, model),
      pop = factor(pop)
    )
  locs <- dplyr::filter(locs, time %in% sort(unique(
    c(min(time),
      time[seq(1, length(time), length.out = steps)],
      max(time))
  )))

  # convert pixel-based coordinates to the internal CRS
  locs <- reproject(
    coords = locs,
    from = "raster", to = "world",
    model = model,
    add = TRUE
  )

  p <- plot_map(model$world) +
    geom_point(data = locs, aes(newx, newy, color = pop), alpha = 0.5, size = 0.5) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank())

  if (length(unique(locs$pop)) == 1)
    p <- p + theme(legend.position = "none")

  if (model$direction == "backward") {
    transition <- gganimate::transition_states(
      -time,
      transition_length = 1,
      state_length = 0
    )
  } else {
    transition <- gganimate::transition_states(
      time,
      transition_length = 1,
      state_length = 0
    )
  }

  gganim <- p + transition + ggtitle("time: {abs(as.integer(closest_state))}")

  anim <- gganimate::animate(
    gganim,
    nframes = length(unique(locs$time)),
    width = width,
    height = height
  )

  if (is.null(gif))
    return(anim)
  else
    gganimate::anim_save(gif, anim)
}

#' Plot locations of ancestors of given individual or node on a map
#'
#' @param data \code{pyslim.SlimTreeSequence} object
#' @param x Either a string representing an individual name, or an integer
#'   number specifying a node in a tree sequence. If \code{NULL} (default), the
#'   spatial ancestry of all focal nodes will be plotted.
#' @param full_scale Plot time gradient on the full scale (spanning the oldest
#'   sampled individual to the present)
#' @param younger_than Time boundaries for the samples
#' @param color How to color connecting lines? Allowed values are either
#'   \code{"time"} (continuous variable specifying the age of the parental node)
#'   or \code{"level"} (factor variable indicating the number of coalescent
#'   events separating the 'focal node' and the given ancestral node).
#'
#' @export
plot_ancestors <- function(data, x = NULL, full_scale = TRUE,
                           younger_than = NULL, color = c("time", "level")) {
  model <- attr(data, "model")
  color <- match.arg(color)

  # if specified, narrow down samples to a given time window
  comp_op <- ifelse(model$direction == "backward", `>`, `<`)
  if (!is.null(younger_than)) data <- dplyr::filter(data, !comp_op(parent_time, younger_than))

  # the name of a sampled individual was specified
  if (is.null(x))
    ids <- unique(data$node_id)
  else if (is.character(x)) {
    if (!all(x %in% data$name))
      stop("Unknown individual ", x[!x %in% data$name], " in the time range of the given data",
           call. = FALSE)
    ids <- dplyr::filter(data, name %in% x)$node_id %>% unique
  } else if (is.numeric(x)) {
    if (!all(x %in% data$node_id))
      stop("Unknown node ", x[!x %in% data$node_id], "in the time range of the given data",
           call. = FALSE)
    ids <- dplyr::filter(data, node_id %in% x)$node_id
  } else
    stop("Unknown object given as an individual or a node", call. = FALSE)

  # add labels for facets (also labeled with individual names if individuals are
  # being plotted)
  data <- dplyr::filter(data, node_id %in% ids) %>%
    dplyr::mutate(label = paste("focal node", node_id))
  if (is.character(x)) {
    ind_labels <- dplyr::as_tibble(data) %>%
      dplyr::filter(!is.na(name)) %>%
      dplyr::distinct(name, node_id)
    data <- dplyr::mutate(data, label = unlist(purrr::map2(
      node_id, label,
      ~ sprintf("%s (%s)", .y, ind_labels[ind_labels$node_id == .x, ]$name)
    )))
  }

  # extract the focal individual or node
  focal_node <- dplyr::filter(data, node_id %in% ids, level == 1) %>%
    dplyr::distinct(node_id, .keep_all = TRUE) %>%
    sf::st_as_sf() %>%
    sf::st_set_geometry("child_location")

  link_aes <- if (color == "time") aes(color = parent_time) else aes(color = level)
  if (color == "level")
    color_scale <- scale_color_discrete(breaks = round(seq(1, max(as.integer(data$level)), length.out = 5)))
  else
    color_scale <- scale_color_continuous(type = "viridis", trans = "reverse")

  ggplot() +
    # world map
    geom_sf(data = model$world, fill = "lightgray", color = NA) +

    # links between nodes ("spatial branches")
    geom_sf(data = data, link_aes, size = 0.5, alpha = 0.75) +

    # focal individual (or)
    geom_sf(data = focal_node, shape = 13, size = 3, color = "red") +

    # all ancestral nodes
    geom_sf(data = sf::st_set_geometry(data, "parent_location"),
            aes(shape = parent_pop), alpha = 0.5) +

    coord_sf(expand = 0) +
    theme_bw() +
    theme(legend.position = "right") +
    guides(shape = guide_legend("population")) +
    facet_wrap(~ label) +
    ggtitle("Spatio-temporal placement of nodes ancestral to a given focal node") +
    color_scale
}

# helper functions --------------------------------------------------------

# Create a table of population split edges for graph visualization
get_split_edges <- function(split_table) {
  split_edges <- split_table[split_table$parent != "ancestor",
                             c("parent", "pop", "tsplit")]
  names(split_edges) <- c("from", "to", "time")

  if (!nrow(split_edges)) return(split_edges)

  split_edges$type <- "split"
  split_edges$rate <- NA
  split_edges$x <- paste0(split_edges$from, "#####", split_edges$time)
  split_edges$y <- paste0(split_edges$to, "#####", split_edges$time)

  split_edges <- split_edges[, c("x", "y", "type", "time", "rate")]
  rownames(split_edges) <- NULL

  split_edges
}


# Create a table of population geneflow edges for graph visualization
get_geneflow_edges <- function(admix_table) {
  if (is.null(admix_table)) {
    admix_edges <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(admix_edges) <- c("from", "to", "rate", "tstart")
    return(admix_edges)
  }

  admix_edges <- admix_table[, c("from", "to", "rate", "tstart")]
  names(admix_edges) <- c("from", "to", "rate", "time")
  admix_edges$type <- "geneflow"
  admix_edges$rate <- sprintf("%.1f%%", admix_edges$rate * 100)
  admix_edges$x <- paste0(admix_edges$from, "#####", admix_edges$time)
  admix_edges$y <- paste0(admix_edges$to, "#####", admix_edges$time)

  admix_edges[, c("x", "y", "type", "time", "rate")]
}


# Create a table of 'intermediate' edges for graph visualization
#
# For plotting the entire geneflow graph, just nodes representing population
# splits are not enough. We also need nodes (population states) which are not
# explicitly simulated as separate population, but they represent time points
# needed to plot geneflow edges.
get_intermediate_edges <- function(split_edges, geneflow_edges) {
  edges <- rbind(split_edges, geneflow_edges)

  all_nodes <- c(edges$x, edges$y)

  intermediate_nodes <- lapply(
    unique(gsub("#####.*$", "", all_nodes)),
    function(i) {
      # grab all nodes and their times belonging to the current population i
      nodes <- all_nodes[grepl(paste0(i, "#####"), all_nodes)]
      times <- gsub(".*#####", "", nodes)
      # leave out the ancestral node/time from the sorting (in case it is even
      # present as a node with a non-integer "ancestral" time)
      ancestor <- nodes[times == "ancestral"]
      nodes <- nodes[times != "ancestral"]
      times <- as.integer(times[times != "ancestral"])
      c(ancestor, unique(nodes[order(times, decreasing = TRUE)]))
    }
  )

  lapply(intermediate_nodes, function(nodes) {
    if (length(nodes) == 1) return(NULL)
    # construct data frame of consecutive, linked pairs of nodes
    pairs <- cbind(nodes[-length(nodes)], nodes[-1]) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      stats::setNames(c("x", "y"))
    pairs$type <- "intermediate"
    pairs$time <- as.integer(gsub(".*#####", "", pairs$y))
    pairs$rate <- NA
    pairs
  }) %>%
  do.call(rbind, .)
}


# Get table of node labels in the graph
get_graph_nodes <- function(edges) {
  nodes <- unique(c(edges$x, edges$y))

  # find the first occurrence of a population in the model based
  # on the time label
  pops <- gsub("#####.*$", "", nodes)
  times <- as.integer(gsub("^.*#####", "", nodes))

  ordered <- order(times, decreasing = TRUE)

  # non-duplicated elements in the vector of population names
  # indicate the first position of such element
  nodes <- data.frame(
    name = nodes[ordered],
    pop = pops[ordered],
    time = times[ordered],
    first = !duplicated(pops[ordered]),
    stringsAsFactors = FALSE
  )
  # assign a type to each node based on the corresponding edge type
  nodes$type <- sapply(nodes$name, function(i) {
    type <- grep("intermediate", edges[edges$y == i, ]$type, invert = TRUE, value = TRUE)
    if (length(type) > 0)
      return(unique(type))
    else
      return("intermediate")
  })
  nodes$type[!nodes$name %in% edges$y] <- "ancestral"

  nodes <- nodes %>% dplyr::mutate(label = dplyr::case_when(
    type == "split" ~ sprintf("%s split\nat %s", pop, time),
    type == "ancestral" ~ paste(pop, "(ancestor)"),
    type == "geneflow" ~ sprintf("geneflow\nat %s", time),
    type == "intermediate" ~ sprintf("from %s", pop)
  ))
  nodes
}

