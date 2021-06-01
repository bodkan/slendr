#' Animate the simulated population dynamics
#'
#' @param model Compiled \code{slendr_model} model object
#' @param locations Table with individual locations
#' @param nframes Number of frames of the animation
#' @param gif Path to an output GIF (animation object returned by default)
#' #' @param nframes Number of frames of the animation
#'
#' @return If `gif = NULL`, return gganimate animation object. Otherwise a GIF
#'   file is saved and no value is returned.
#'
#' @import ggplot2
#' @export
animate <- function(model, locations = NULL, nframes = 200, gif = NULL) {
  if (is.null(locations))
    locations <- file.path(model$config$directory, "output_ind_locations.tsv.gz")
  locs <- read.table(locations, header = TRUE)
  pop_names <- scan(file.path(model$config$directory, "names.txt"), what = "character", quiet = TRUE)

  # label populations based on their original idenifiers from the user
  locs$pop <- factor(
    locs$pop,
    levels = sort(unique(locs$pop)),
    labels = pop_names[sort(unique(locs$pop)) + 1]
  )
  locs$tyears <- as.integer(locs$t * model$generation_time)

  # cut time into blocks - same as the number of frames of the final GIF
  locs$tblock <- cut(
    locs$tyears,
    breaks = seq(max(locs$tyears), min(locs$tyears), length.out = nframes + 1),
    include.lowest = TRUE,
    dig.lab = 10
  )

  # reverse the order of time in the forward direction
  ordered <- rev(levels(locs$tblock[order(locs$tyears)]))
  locs$tblock <- factor(locs$tblock, levels = ordered)

  # lower sample density for plotting
  #locs <- dplyr::sample_n(locs, 10000)

  # convert pixel-based coordinates to real projected CRS coordinates
  locs <- convert(
    coords = locs,
    from = "raster", to = "world",
    model = model,
    add = TRUE
  )

  p <- plot(model$world) +
    geom_point(data = locs, aes(newx, newy, color = pop), alpha = 0.5) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())

  anim <- p +
    gganimate::transition_states(
      tblock,
      transition_length = 1,
      state_length = 0
    ) +
    ggtitle("time block: {closest_state} years ago")

  anim <- gganimate::animate(
    anim,
    nframes = nframes + 1,
    width = 800,
    height = 560
  )

  if (is.null(gif))
    return(anim)
  else
    gganimate::anim_save(gif, anim)
}


#' Plot simulated ancestry proportions
#'
#' @param model Compiled \code{slendr_model} model object
#'
#' @export
ancestries <- function(model, generation_time = FALSE) {
  anc_wide <- read_ancestries(model$config$directory)

  # thank god for tidyverse, base R reshaping is truly awful...  but
  # it's not worth dragging along a huge dependency if we can do this
  # sort of thing in base R...
  anc_long <- reshape(
    data = anc_wide,
    direction = "long",
    timevar = "ancestry",
    varying = 2:(ncol(anc_wide) - 1),
    idvar = c("gen", "pop"),
    v.names = "prop",
    times = colnames(anc_wide)[2:(ncol(anc_wide) - 1)]
  )
  rownames(anc_long) <- NULL

  # order population names based on their split order
  split_order <- split(anc_long, anc_long$pop) %>%
    sapply(function(df) max(df$gen)) %>%
    sort(decreasing = TRUE) %>%
    names
  anc_long$pop <- factor(anc_long$pop, levels = split_order)

  if (generation_time)
    anc_long$time <- anc_long$gen
  else
    anc_long$time <- anc_long$gen * model$generation_time

  anc_long %>%
  ggplot(aes(-time, prop, color = ancestry)) +
    geom_line() +
    facet_wrap(~ pop) +
    coord_cartesian(ylim = c(0, 1)) +
    ggtitle("Ancestry proportions in populations during the course of their existence") +
    theme_minimal()
}


#' Plot geneflow graph based on given model configuration
#'
#' @param model Compiled \code{slendr_model} model object
#' @param show_cleanups Show nodes indicating the times of population
#'   removals?
#'
#' @import ggplot2 ggraph
#' @export
graph <- function(model, show_cleanups = TRUE) {
  # summarize model configuration into a tabular form
  split_table <- model$splits
  geneflow_table <- model$geneflows

  split_edges <- get_split_edges(split_table)
  geneflow_edges <- get_geneflow_edges(geneflow_table)
  terminal_edges <- get_terminal_edges(split_edges, geneflow_edges, split_table)
  intermediate_edges <- get_intermediate_edges(split_edges, geneflow_edges)

  edges <- rbind(
    split_edges,
    geneflow_edges,
    terminal_edges,
    intermediate_edges
  )

  if (!show_cleanups)
    edges <- edges[edges$type != "terminal", ]

  nodes <- get_nodes(edges)

  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

  ggraph(g, layout = "sugiyama") +

  # geneflow edges along with geneflow rates
  geom_edge_link(
    aes(filter = type == "geneflow", label = rate,
        start_cap = label_rect(node1.name),
        end_cap = label_rect(node2.name),
        linetype = "geneflow"),
    angle_calc = "along",
    label_dodge = unit(3, "mm"),
    arrow = arrow(length = unit(4, "mm"))
  ) +

  # population split/continuation edges (no rates labeled)
  geom_edge_link(
    aes(filter = type  == "split",
        start_cap = label_rect(node1.name),
        end_cap = label_rect(node2.name),
        linetype = "split"),
    label_dodge = unit(10, "mm"),
    arrow = arrow(length = unit(4, "mm"))
  ) +

  # continuation edges
  geom_edge_link(aes(filter = type == "intermediate",
                     linetype = "continuation")) +

  # population removal time stamp
  geom_edge_link(aes(filter = type == "terminal"), alpha = 0.25) +

  geom_node_label(aes(filter = type == "split", fill = pop,
                      label = sprintf("%s split at %s", pop, time))) +

  geom_node_label(aes(filter = type == "ancestral", fill = pop,
                      label = paste(pop, "(ancestor)")),
                  label.size = 1) +

  geom_node_label(aes(filter = type == "geneflow", fill = pop,
                      label = sprintf("geneflow at %s", time))) +

  geom_node_label(aes(filter = type == "intermediate", fill = pop,
                      label = sprintf("from %s", pop))) +

  geom_node_label(aes(filter = type == "terminal",
                      label = sprintf("removed\nat %s",
                                        ifelse(time == 0, "the end", time))),
                  fill = "white") +

  scale_edge_linetype_manual(values = c("split" = "solid",
                                        "continuation" = "solid",
                                        "geneflow" = "solid")) +

  guides(fill = guide_legend(""), edge_linetype = FALSE) +

  theme_void() +
  theme(legend.position = "right",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.justification = "top") +
  coord_cartesian(clip = "off")
}


#' Create a table of population split edges for graph visualization
get_split_edges <- function(split_table) {
  split_edges <- split_table[split_table$parent != "ancestor",
                             c("parent", "pop", "tsplit")]
  names(split_edges) <- c("from", "to", "time")

  if (!nrow(split_edges)) return(split_edges)

  split_edges$type <- "split"
  split_edges$rate <- NA
  split_edges$x <- paste0(split_edges$from, "-", split_edges$time)
  split_edges$y <- paste0(split_edges$to, "-", split_edges$time)

  split_edges <- split_edges[, c("x", "y", "type", "time", "rate")]
  rownames(split_edges) <- NULL

  split_edges
}


#' Create a table of population geneflow edges for graph visualization
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
  admix_edges$x <- paste0(admix_edges$from, "-", admix_edges$time)
  admix_edges$y <- paste0(admix_edges$to, "-", admix_edges$time)

  admix_edges[, c("x", "y", "type", "time", "rate")]
}


#' Create a table of 'intermediate' edges for graph visualization
#'
#' For plotting the entire geneflow graph, just nodes representing population
#' splits are not enough. We also need nodes (population states) which are not
#' explicitly simulated as separate population, but they represent time points
#' needed to plot geneflow edges.
get_intermediate_edges <- function(split_edges, geneflow_edges) {
  edges <- rbind(split_edges, geneflow_edges)

  all_nodes <- c(edges$x, edges$y)

  intermediate_nodes <- lapply(
    unique(gsub("-.*$", "", all_nodes)),
    function(i) {
      # grab all nodes and their times belonging to the current population i
      nodes <- all_nodes[grepl(paste0(i, "-"), all_nodes)]
      times <- gsub(".*-", "", nodes)
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
      setNames(c("x", "y"))
    pairs$type <- "intermediate"
    pairs$time <- as.integer(gsub(".*-", "", pairs$y))
    pairs$rate <- NA
    pairs
  }) %>% do.call(rbind, .)
}


#' Generate edges leading to the times of population removals in SLiM
get_terminal_edges <- function(split_edges, geneflow_edges, split_table) {
  edges <- rbind(split_edges, geneflow_edges)

  # iterate over all populations and create a data frame of edges from the
  # population split nodes, to the nodes of the population removal from the
  # simulation
  terminal_edges <- lapply(1:nrow(split_table), function(i) {
    pop <- split_table[i, ]

    times <- edges[grepl(pop$pop, edges$x) | grepl(pop$pop, edges$y), ]$time
    prev_time <- if (length(times)) max(times) else .Machine$integer.max
    prev_node <- paste0(pop$pop, "-", prev_time)
    tremove <- if (pop$tremove == -1) 0 else pop$tremove

    data.frame(
      x = prev_node,
      y = paste0(pop$pop, "-", tremove),
      type = "terminal",
      time = tremove,
      rate = NA,
      stringsAsFactors = FALSE
    )
  }) %>% do.call(rbind, .)

  terminal_edges
}


#' Get table of node labels in the graph
get_nodes <- function(edges) {
  nodes <- unique(c(edges$x, edges$y))

  # find the first occurrence of a population in the model based
  # on the time label
  pops <- gsub("-.*$", "", nodes)
  times <- as.integer(gsub("^.*-", "", nodes))

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
      return(type)
    else
      return("intermediate")
  })
  nodes$type[!nodes$name %in% edges$y] <- "ancestral"
  nodes
}


#' Plot \code{slendr} geographic features on a map
#'
#' Plots objects of the three \code{slendr} spatial classes
#' (\code{slendr_map}, \code{slendr_region}, and \code{slendr_pop}).
#'
#' @param ... Objects of classes \code{slendr_map},
#'   \code{slendr_region}, or \code{slendr_pop}
#' @param time Plot a concrete time point
#' @param geneflows Indicate geneflow events with an arrow
#' @param graticules Plot graticules in the original Coordinate
#'   Reference System (such as longitude-latitude), or in the internal
#'   CRS (such as meters)?
#' @param intersect Intersect the population boundaries against
#'   landscape and other geographic boundaries (default TRUE)?
#' @param show_map Show the underlying world map
#' @param title Title of the plot
#'
#' @export
#'
#' @import ggplot2
plot.slendr <- function(..., time = NULL, geneflows = FALSE,
                        graticules = "original",
                        intersect = TRUE, show_map = TRUE,
                        interpolated_maps = NULL, title = NULL) {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

  if (is.null(show_map)) show_map <- FALSE

  args <- list(...)

  if(!all(sapply(args, inherits, "slendr")))
    stop("Only objects of the class 'slendr' can be visualized using plot.slendr", call. = FALSE)

  classes <- grep("slendr_", unique(unlist(sapply(args, class))), value = TRUE)
  if (length(classes) > 1 & "slendr_model" %in% classes)
    stop("If a 'slendr_model' object is to be plotted, it must be a single argument", call. = FALSE)
  if (length(classes) > 1 & "slendr_map" %in% classes)
    stop("If a 'slendr_map' object is to be plotted, it must be a single argument", call. = FALSE)
  if (length(intersect(c("slendr_region", "slendr_pop"), classes)) > 1)
    stop("'slendr_region' and 'slendr_pops' object cannot be plotted at once", call. = FALSE)

  if (geneflows & (is.null(time) | !inherits(args[[1]], "slendr_model")))
    stop("Migrations can be visualized only when a time point *and* a 'slendr_model' objects are specified", call. = FALSE)

  # a single model object was provided
  if (length(args) == 1 & inherits(args[[1]], "slendr_model")) {
    model <- args[[1]]
    pops <- model$populations
    map <- model$world
    regions <- list()
  } else if (all(classes == "slendr_map")) {
    map <- args[[1]]
    regions <- pops <- list()
  } else {
    # extract the map component underlying each population object
    # and make sure they are all the same with no conflicts
    maps <- unique(lapply(args, function(i) attr(i, "map")))
    if (length(maps) != 1) {
      stop("Objects do not share the same map component", call. = F)
    }
    map <- maps[[1]]

    regions <- lapply(args, function(i) if (!is.null(i$region)) i) %>% Filter(Negate(is.null), .)
    pops <- lapply(args, function(i) if (!is.null(i$pop)) i) %>% Filter(Negate(is.null), .)
  }

  if (graticules == "original" & has_crs(map)) {
    graticule_crs <- "EPSG:4326"
    xlab <- "degrees longitude"; ylab <- "degrees latitude"
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

  if (nrow(map) & show_map)
    p <- p + geom_sf(data = map, aes(frame = NULL), fill = "lightgray", color = NA)

  if (length(pops)) {
    pop_names <- unique(unlist(sapply(pops, `[[`, "pop")))

    # if the user specified a time point, "interpolate" all maps at that
    # time and return just those that match that time point (unless this
    # was already pre-computed)
    if (!is.null(time)) {
      if (is.null(interpolated_maps))
        interpolated_maps <- fill_maps(pops, time)

      # get all time points defined by the user
      all_times <- sort(unique(unlist(lapply(pops, `[[`, "time"))))

      # get split and removal times of all specified populations
      split_times <- sapply(sapply(pops, `[[`, "time"), `[`, 1)
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
      warning("Attempting to plot population ranges at multiple time points on
a single map. This is very hard to do in a satisfying way. Please
consider using the function `explore()` to plot the model dynamics
interactively.", call. = FALSE)
      # build a base map with geographic features
      p <- p +
        geom_sf(data = pop_maps, aes(fill = pop, alpha = time), color = NA) +
        geom_sf(data = pop_maps, fill = NA, color = "black", size = 0.1)
    } else {
      p <- p +
        geom_sf(data = pop_maps, aes(fill = pop), color = NA, alpha = 0.4) +
        geom_sf(data = pop_maps, fill = NA, color = "black", size = 0.1)
    }
    p <- p + scale_fill_discrete(drop = FALSE, name = "")

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
            arrow = arrow(length = unit(2,"mm"), type = "closed"),
            lineend = "round", size = 0.5, arrow.fill = "black"
          ) +
          scale_color_discrete(drop = FALSE) +
          guides(color = FALSE)
    }
  }

  if (length(regions)) {
    region_maps <- do.call(rbind, regions)
    p <- p +
      geom_sf(data = region_maps, aes(fill = region), linetype = 2, alpha = 0.5) +
      geom_sf_label(data = region_maps, aes(label = region, color = region)) +
      guides(color = FALSE, fill = FALSE)
  }

  if (!is.null(title)) p <- p + ggtitle(title)

  p + labs(x = xlab, y = ylab) +
    theme_bw() +
    p_coord
}
