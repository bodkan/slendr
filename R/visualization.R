#' Plot \code{slendr} geographic features on a map
#'
#' Plots objects of the three \code{slendr} spatial classes (\code{slendr_map},
#' \code{slendr_region}, and \code{slendr_pop}).
#'
#' @param ... Objects of classes \code{slendr_map}, \code{slendr_region}, or
#'   \code{slendr_pop}
#' @param time Plot a concrete time point
#' @param gene_flow Indicate geneflow events with an arrow
#' @param labels Should the (starting) polygons of each populations be labeled with
#'   a respective population label (default \code{FALSE})?
#' @param graticules Plot graticules in the original Coordinate Reference System
#'   (such as longitude-latitude), or in the internal CRS (such as meters)?
#' @param intersect Intersect the population boundaries against landscape and
#'   other geographic boundaries (default \code{TRUE})?
#' @param show_map Show the underlying world map
#' @param title Title of the plot
#' @param interpolated_maps Interpolated spatial boundaries for all populations
#'   in all time points (this is only used for plotting using the \code{explore}
#'   shiny app)
#'
#' @return A ggplot2 object with the visualized slendr map
#' @importFrom ggplot2 coord_sf geom_sf guides guide_legend theme_bw geom_sf_label
#' @export
#'
plot_map <- function(..., time = NULL, gene_flow = FALSE,
                     labels = FALSE,
                     graticules = "original",
                     intersect = TRUE, show_map = TRUE,
                     title = NULL, interpolated_maps = NULL) {
  check_spatial_pkgs()

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

  if (gene_flow & (is.null(time) | !inherits(args[[1]], "slendr_model")))
    warning("All gene-flow event will be visualized at once. If you wish to visualize\n",
            "gene flows at a particular point in time, use the `time` argument.", call. = FALSE)

  # a single model object was provided
  if (length(args) == 1 & inherits(args[[1]], "slendr_model")) {
    model <- args[[1]]
    pops <- model$populations
    map <- model$world
    regions <- list()
    direction <- time_direction(model)
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

    direction <- setdiff(unique(sapply(pops, time_direction)), "unknown")
    if (!length(direction)) direction <- "forward"
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
    pop_names <- order_pops(pops, direction)
    if (any(duplicated(pop_names)))
      stop("Duplicated population names within a single model are not allowed", call. = FALSE)

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

      # get only those populations already/still present at the
      # specified time...
      if (direction == "forward") {
        previous_time <- max(all_times[all_times <= time])
        present_pops <- interpolated_maps[split_times <= time & (removal_times == -1 | removal_times >= time)]
      } else {
        previous_time <- min(all_times[all_times >= time])
        present_pops <- interpolated_maps[split_times >= time & (removal_times == -1 | removal_times <= time)]
      }

      # ... and extract their spatial maps
      pop_maps <- lapply(present_pops, function(pop) {
        snapshot <- pop[pop$time == previous_time, ]
        attributes(snapshot) <- attributes(pop)
        snapshot
      })
    } else {
      pop_maps <- pops
    }

    if (intersect) {
      intersected_maps <- pop_maps %>% Filter(has_map, .) %>% lapply(intersect_features)
      if (length(intersected_maps) != length(pop_maps))
        warning("Non-spatial populations in your model won't be visualized", call. = FALSE)
      pop_maps <- intersected_maps
    }

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

    p <- p + scale_fill_discrete(drop = FALSE, name = "") +
      guides(alpha = guide_legend("time"))

    # add geneflow arrows, if requested
    if (gene_flow) {
      migr_df <- get_geneflows(model, time)
      if (nrow(migr_df))
        p <- p +
          geom_point(data = migr_df, aes(x = from_x, y = from_y, color = from), size = 7) +
          geom_point(data = migr_df, aes(x = to_x, y = to_y, color = to), size = 7) +
          geom_segment(
            data = migr_df,
            aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
            arrow = arrow(length = unit(2, "mm"), type = "closed"),
            lineend = "round", linewidth = 0.5, arrow.fill = "black"
          ) +
          scale_color_discrete(drop = FALSE) +
          guides(color = "none")
    }

    if (labels) {
      p <- p +
        ggrepel::geom_label_repel(data = dplyr::group_by(pop_maps, pop, time) %>% dplyr::arrange(time) %>% dplyr::slice(1),
                      aes(label = pop, color = pop, geometry = geometry), stat = "sf_coordinates")
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

# Traverse the tree topology encoded by population splits in an in-order recursive fashion,
# return encountered populations sorted in this way
sort_inorder <- function(splits, root) {
  # get all populations splitting from the current root (not necessarily the
  # real root of the whole population phylogeny, but a root of a current "subtree" in
  # a recursive sense)
  children <- splits[splits$parent == root, ]$pop

  # terminating condition for the recursion
  if (!length(children))
    return(root)

  # collect in-order sorted leaves below children of the current root
  result <- c()
  for (pop in children) {
    result <- c(result, sort_inorder(splits, pop))
  }

  # if multiple daughter populations split from the current population, interleave their
  # splits left and right from that population
  if (length(result) > 1) {
    left_pops <- result[seq(1, length(result), 2)]
    right_pops <- result[seq(2, length(result), 2)]
    sorted <- c(left_pops, root, right_pops)
  } else {
    sorted <- c(result, root) # otherwise put the sole splitting daughter population to the left
  }

  # return recursively as a list (it will eventually be flattened on the top level in
  # the sort_splits function)
  list(sorted)
}

sort_splits <- function(model) {
  # extract names of all ancestral populations from the split table
  splits <- model$splits
  ancestors <- subset(splits, parent == "__pop_is_ancestor")$pop

  # iterate over all ancestors (i.e., roots of individual phylogenies if the whole model
  # is not rooted under a single ancestral population) and flatten the nested lists
  # (if this is done with recursive = TRUE, the sorted order of populations will be retained
  # which is what we want!)
  lineage_splits <- lapply(ancestors, function(x) unlist(sort_inorder(splits, x), recursive = TRUE))

  # concatenate all sublineages into a single vector of population names
  do.call(c, lineage_splits)
}

#' Plot demographic history encoded in a slendr model
#'
#' @param model Compiled \code{slendr_model} model object
#' @param sizes Should population size changes be visualized?
#' @param proportions Should gene flow proportions be visualized (\code{FALSE}
#'   by default to prevent cluttering and overplotting)
#' @param gene_flow Should gene-flow arrows be visualized (default \code{TRUE}).
#' @param log Should the y-axis be plotted on a log scale? Useful for models
#'   over very long time-scales.
#' @param order Order of the populations along the x-axis, given as a character
#'   vector of population names. If \code{NULL} (the default), the default plotting
#'   algorithm will be used, ordering populations from the most ancestral to the
#'   most recent using an in-order tree traversal.
#' @param file Output file for a figure saved via \code{ggsave}
#' @param samples Sampling schedule to be visualized over the model
#' @param ... Optional argument which will be passed to \code{ggsave}
#'
#' @return A ggplot2 object with the visualized slendr model
#'
#' @examples
#' \dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#' init_env()
#'
#' # load an example model with an already simulated tree sequence
#' path <- system.file("extdata/models/introgression", package = "slendr")
#' model <- read_model(path)
#'
#' plot_model(model, sizes = FALSE, log = TRUE)
#' @importFrom ggplot2 ggplot expand_limits theme_classic element_line unit
#'   geom_polygon geom_label scale_y_continuous scale_color_discrete scale_fill_discrete
#'   labs geom_segment arrow
#' @export
plot_model <- function(model, sizes = TRUE, proportions = FALSE, gene_flow = TRUE, log = FALSE,
                       order = NULL, file = NULL, samples = NULL, ...) {
  populations <- model$populations

  log10_ydelta <- 0.001

  # layout populations along the x-axis according to an in-order population tree traversal
  if (is.null(order))
    pop_names <- sort_splits(model)
  else {
    pop_names <- order
    if (length(setdiff(vapply(populations, function(pop) pop$pop[1], FUN.VALUE = character(1)),
                       pop_names)))
      stop("If order is given manually, all population names must be specified", call. = FALSE)
  }

  split_times <- vapply(pop_names, function(x) attr(populations[[x]], "history")[[1]]$time,
                        numeric(1))

  pop_factors <- order_pops(model$populations, model$direction)

  # extract times at which each population will be removed from the simulation
  if (model$direction == "backward")
    default_end <- log10_ydelta
  else
    default_end <- get_oldest_time(model$populations, model$direction) + model$orig_length

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
    pop = factor(pop_names, levels = pop_factors),
    N = final_sizes,
    time = split_times,
  ) %>%
    dplyr::mutate(xmax = cumsum(N + stats::median(N)),
                  xmin = xmax - N,
                  center = xmin + N / 2) %>%
    dplyr::select(pop, center, time) %>%
    dplyr::mutate(center = center - center[1] / 2)

  # iterate over each population's demographic history and compose the
  # coordinates of polygons in each epoch
  size_changes <- lapply(populations, function(pop) {
    # pop <- populations[[i]]
    name <- pop$pop[1]
    center <- centers[centers$pop == name, ]$center

    # collect all historical events from the present to the past
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
      else if (next_event$event == "resize" && next_event$how == "step")
        ys <- c(ys, rep(next_event$tresize, 2))
      else if (next_event$event == "resize" && next_event$how == "exponential")
        ys <- c(ys, next_event$tend, rep(next_event$tresize, 2), next_event$tend)
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
        xs <- c(xs,
                center + next_event$N / 2, center + next_event$prev_N / 2,
                center - next_event$prev_N / 2, center - next_event$N / 2)
      else
        stop("Invalid 'next event'. This is a slendr bug! (4)", call. = FALSE)

      dplyr::tibble(
        pop = factor(name, levels = pop_factors),
        x = xs,
        y = ifelse(ys == 0, log10_ydelta, ys)
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
        pop = factor(pop_name, levels = pop_factors),
        from = factor(parent_name, levels = pop_factors),
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
  if (!is.null(model$geneflow) && gene_flow) {
    gene_flow <- model$geneflow %>%
      dplyr::mutate(
        x = purrr::map_dbl(from, ~ centers[centers$pop == .x, ]$center),
        xend = purrr::map_dbl(to, ~ centers[centers$pop == .x, ]$center),
        y = tstart_orig,
        yend = tend_orig
      )
  } else
    gene_flow <- NULL

  ylabel <- "time"
  if (model$time_units != "") ylabel <- sprintf("%s (%s)", ylabel, model$time_units)

  if (log) ylabel <- paste(ylabel, "(log scale)")

  # setup a figure outline
  p <- ggplot()  +
    scale_color_discrete(drop = FALSE) +
    scale_fill_discrete(drop = FALSE) +
    labs(y = ylabel) +
    expand_limits(x = 0) +
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

  # add horizontal split lines
  if (!is.null(splits)) {
    p <- p + geom_segment(
      data = splits,
      aes(x = x, xend = xend, y = y, yend = yend, color = pop),
      linewidth = 1
    )
  }

  # add each each epoch resize segment
  for (lineage in size_changes) {
    for (event in lineage)
      p <- p + geom_polygon(data = event, aes(x, y, fill = pop))
  }

  # labels with population names
  # except for outgroups and populations with two or more daughter populations, all
  # population labels will be plotted at the bottom of the figure
  # (not currently done because of issues with overplotting with sampling labels)
  # end_labels <- model$splits[
  #   model$splits$parent != "__pop_is_ancestor" &
  #   vapply(model$splits$pop, function(x) sum(x == model$splits$parent), integer(1)) < 2
  # , ]$pop
  # centers[centers$pop %in% end_labels, ]$time[end_labels] <- end_times[end_labels] + log10_ydelta
  p <- p + geom_label(data = centers, size = 3,
                      aes(label = pop, x = center, y = time, fill = pop), fontface = "bold")

  # add gene flow arrows and proportion labels
  if (!is.null(gene_flow)) {
    p <- p + geom_segment(data = gene_flow,
                          aes(x = x, xend = xend, y = y, yend = yend + log10_ydelta),
                          arrow = arrow(length = unit(0.2, "cm")))
    p <- p + geom_point(data = gene_flow, aes(x = x, y = y))
    if (proportions) {
      p <- p + geom_label(data = gene_flow,
                          aes(label = sprintf("%s%%", 100 * rate),
                              x = xend - (xend - x) / 2,
                              y = y + (yend + log10_ydelta - y) / 2), size = 3)
    }
  }

  if (model$direction == "forward") {
    if (log)
      trans <- scales::compose_trans(scales::log10_trans(), scales::reverse_trans())
    else
      trans <- scales::reverse_trans()
  } else if (log)
    trans <- scales::log10_trans()
  else
    trans <- scales::identity_trans()

  # make sure that the y-axis of a model is truncated exactly at the start and end time
  # (it seems that the first rectangle that is drawn is plotted even "earlier" than a
  # model start but this takes care of things for now -- if more plotting issues pop up,
  # they can be fixed later)
  oldest_time <- get_oldest_time(model$populations, model$direction)
  if (model$direction == "forward") {
    ylim_low <- get_oldest_time(model$populations, model$direction)
    ylim_high <- oldest_time + model$orig_length
    ylim <- c(ylim_high, ylim_low)
  } else {
    ylim_high <- get_oldest_time(model$populations, model$direction)
    ylim_low <- oldest_time - model$orig_length
    ylim <- c(ylim_low, ylim_high)
  }
  ylim[ylim == 0] <- log10_ydelta

  p <- p + scale_y_continuous(limits = ylim, trans = trans)

  # if specified, overlay sampling points over the model
  if (!is.null(samples)) {
    sampling_points <- dplyr::select(centers, pop, center) %>%
      dplyr::inner_join(samples, by = "pop") %>%
      dplyr::mutate(time = ifelse(time == 0, log10_ydelta, time))
    p <- p + geom_label(data = sampling_points, aes(label = n, x = center, y = time),
                   fontface = "bold", alpha = 0.5)
  }

  if (!is.null(file))
    ggplot2::ggsave(file, plot = p, ...)
  else
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
#' @importFrom ggplot2 geom_point aes theme element_blank ggtitle
#' @export
animate_model <- function(model, file, steps, gif = NULL, width = 800, height = 560) {
  check_spatial_pkgs()

  if (!requireNamespace("magick", quietly = TRUE))
    message("For rendering animated GIFs, please install the R package ",
            "magick by calling `install.packages(\"magick\")`")

  if (!file.exists(file))
    stop("Could not find file with locations at", file, call. = FALSE)

  if (!inherits(model$world, "slendr_map"))
    stop("Cannot animate non-spatial models", call. = FALSE)

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
