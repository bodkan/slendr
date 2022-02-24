# Get path to an appropriate SLiM binary
get_binary <- function(method) {
  if (method == "gui") {
    if (Sys.info()["sysname"] == "Darwin")
      binary <- "open -a SLiMgui"
    else
      binary <- "SLiMgui"
  } else
    binary <- "slim"

  binary
}


# Check whether given population region has not yet been intersected
check_not_intersected <- function(pop) {
  if (!is.null(attr(pop, "intersected")))
    stop("An already intersected population range object was provided.
Please provide a range object before it was intersected against a map.",
         call. = FALSE)
}


# Check whether the given value makes sense given the map dimensions
check_resolution <- function(map, val) {
  xrange <- sf::st_bbox(map)[c("xmin", "xmax")]
  yrange <- sf::st_bbox(map)[c("ymin", "ymax")]
  if (diff(xrange) < val | diff(yrange) < val)
    stop(sprintf("Value of %s = %s larger than the overall world size",
                 deparse(substitute(val)), val),
         call. = FALSE)
}


# Set a bounding box of a given object, and return that object again
# (for some reason there's no builtin way to set a bounding box in
# sf <https://twitter.com/TimSalabim3/status/1063099774977667072>)
set_bbox <- function(x, xmin, xmax, ymin, ymax) {
  bbox <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  attr(bbox, "class") <- "bbox"
  attr(sf::st_geometry(x), "bbox") <- bbox
  x
}


# Does the object have a Coordinate Reference System assigned to it?
has_crs <- function(x) {
  !is.na(sf::st_crs(x)$epsg)
}


# Set slendr classes (or fix their priorities if already present)
set_class <- function(x, type) {
  other_classes <- class(x) %>% .[!grepl("^slendr", .)]
  c("slendr", paste0("slendr_", type), other_classes)
}


# Get a data frame of geneflow events active at a given time point
get_geneflows <- function(model, time) {
  if (is.null(model$geneflow)) return(NULL)
  pop_names <- unique(unlist(sapply(model$populations, `[[`, "pop")))

  geneflows <- subset(model$geneflow, tstart_orig >= time & tend_orig <= time)
  geneflows$from <- factor(geneflows$from, levels = pop_names)
  geneflows$to <- factor(geneflows$to, levels = pop_names)

  migr_coords <- lapply(seq_len(nrow(geneflows)), function(row_i) {

    from <- model$populations[pop_names == geneflows[row_i, ]$from][[1]] %>%
      .[.$time >= time, ] %>%
      .[nrow(.), ]

    to <- model$populations[pop_names == geneflows[row_i, ]$to][[1]] %>%
      .[.$time >= time, ] %>%
      .[nrow(.), ]

    from_center <- sf::st_centroid(from) %>% sf::st_coordinates()
    to_center <- sf::st_centroid(to) %>% sf::st_coordinates()

    coords <- as.data.frame(cbind(from_center, to_center), stringsAsFactors = FALSE)
    colnames(coords) <- c("from_x", "from_y", "to_x", "to_y")
    coords

  }) %>%
  do.call(rbind, .)

  cbind(geneflows, migr_coords)
}


# Transfer given set of attributes from one object to another
copy_attributes <- function(to, from, which) {
  for (i in which)
    attr(to, i) <- attr(from, i)
  class(to) <- class(from)
  to
}


# Get split times of populations in the lineage of the given population
get_lineage_splits <- function(x) {
  parent <- attr(x, "parent")
  time <- attr(x, "history")[[1]]$time
  if (is.character(parent))
    return(time)
  else
    return(c(time, get_lineage_splits(parent)))
}


#' Get direction of time implied by the history of the population
#'
#' @param x Object of the class \code{slendr_pop} or \code{slendr_model}
#'
#' @return Either "forward", "backward", or "unknown"
#'
#' @export
time_direction <- function(x) {
  if (inherits(x, "slendr_model")) return(x$direction)

  split_times <- get_lineage_splits(x)

  if (length(split_times) == 1) {
    event_times <- attr(x, "history") %>%
      sapply(function(event) c(event$time, event$tresize, event$tend,
                               event$start, event$end)) %>%
      unlist %>%
      unique %>%
      stats::na.omit()
    if (length(event_times) == 1) {
      removal_time <- attr(x, "remove")
      if (removal_time == -1)
        "unknown"
      else if (all(event_times > removal_time))
        "backward"
      else if (all(event_times < removal_time))
        "forward"
      else
        stop("Inconsistent time direction of population events", call. = FALSE)
    } else if (all(diff(event_times) < 0))
      "backward"
    else
      "forward"
  } else if (all(diff(split_times) > 0))
    "backward"
  else
    "forward"
}


# Check the consistency of the given split time to the parent population
check_split_time <- function(time, parent) {
  parent_time <- attr(parent, "history")[[1]]$time
  direction <- time_direction(parent)
  if (direction == "forward" & time <= parent_time) {
    stop(sprintf("The model implies forward time direction but the specified split
time (%d) is lower than the parent's (%s)",
                 time, parent_time),
         call. = FALSE)
  } else if (direction == "backward" & time >= parent_time) {
    stop(sprintf("The model implies backward time direction but the specified split
time (%s) is higher than the parent's (%s)",
                 time, parent_time),
         call. = FALSE)
  } else if (time == parent_time) {
    stop("Population can be only created after its parent is already present in the simulation", call. = FALSE)
  }
}


# Get time of the very last event currently specified
get_previous_time <- function(pop) {
  sapply(attr(pop, "history"), function(event) {
    c(event$time, event$tresize, event$tstart, event$tend)
  }) %>%
    unlist %>%
    Filter(Negate(is.null), .) %>%
    Filter(Negate(is.na), .) %>%
    utils::tail(1)
}


check_event_time <- function(time, pop) {
  if (length(time) > 1 & time[1] == time[2])
    stop("Start time of the event is equal to the end time of the event", call. = FALSE)

  direction <- time_direction(pop)

  previous_time <- get_previous_time(pop)

  # testing for consistent for ancestral populations (unknown time direction)
  if (direction == "unknown") {
    # there is no information about time direction from ancestral populations,
    # but we can test that all times of the current event are before/after that
    # population's split time
    if (!(all(time >= previous_time) | all(time <= previous_time)))
      stop(sprintf("The new event (time %s) falls both before and after the last active time (%s) for the population",
                   paste(time, collapse = "-"), previous_time), call. = FALSE)

    # if a start-end time window was specified for the event, we can lock in
    # a time direction even for ancestral populations for which the flow of
    # time is otherwise unknown
    if (length(time) > 1) {
      # the first time point implies time going backwards, but the event window
      # implies a forward direction
      if (time[1] < previous_time & time[2] > time[1])
        stop(sprintf("The new event (time %s) implies a forward time direction but the population split time (%d) is higher (indicating backward time direction)",
             paste(time, collapse = "-"), previous_time), call. = FALSE)
      else if (time[1] > previous_time & time[2] < time[1])
        stop(sprintf("The new event (time %s) implies a backward time direction but the population split time (%d) is lower (indicating forward time direction)",
                     paste(time, collapse = "-"), previous_time), call. = FALSE)
    }
  } else if (direction %in% c("backward", "forward")) {
    # direction of an event follows the already established time direction
    if ((direction == "backward" & length(time) > 1 & time[1] < time[2]) |
        (direction == "forward" & length(time) > 1 & time[1] > time[2])) {
      event_direction <- if (time[1] < time[2]) "forward" else "backward"
      stop(sprintf("The new %s event (time %s) is inconsistent with the %s time direction assumed by the model",
                   event_direction, paste0(time, collapse = "-"), direction), call. = FALSE)
    }

    # time of event is consistent with the already established time direction
    if ((direction == "backward" & !all(previous_time >= time)) |
        (direction == "forward" & !all(previous_time <= time))) {
      stop(sprintf("The new event (time %s) pre-dates the last specified active event (%s) which is incompatible with the assumed %s time direction of the model",
                   paste(time, collapse = "-"), previous_time, direction),
           call. = FALSE)
    }
  } else
    stop(sprintf("Unknown time direction %s", direction), call. = FALSE)
}

# Check whether a given population will be present for sampling
# (used exclusively in the sampling() function to avoid situations when
# a user would sample from a population in the same generation that it
# would be created)
check_present_time <- function(time, pop, offset, direction = NULL) {
  if (is.null(direction))
    direction <- time_direction(pop)
  split_time <- get_lineage_splits(pop)[1]

  if (time == split_time |
      (direction == "backward" & time > split_time - offset) |
      (direction == "forward" & time < split_time + offset))
    stop("Population ", pop$pop[1], " is not present at a time ", time, call. = FALSE)
}

check_removal_time <- function(time, pop, direction = NULL) {
  if (is.null(direction))
    direction <- time_direction(pop)
  removal_time <- attr(pop, "remove")

  if (removal_time != -1 & direction == "forward" & any(time > removal_time)) {
    stop(sprintf("The specified event time (%d) is not consistent with the scheduled removal of %s (%s) given the assumed %s time direction",
                 time, pop$pop[1], removal_time, direction),
         call. = FALSE)
  }
  if (removal_time != -1 & direction == "backward" & any(time < removal_time)) {
    stop(sprintf("The specified event time (%d) is not consistent with the scheduled removal of %s (%s) given the assumed %s time direction",
                 time, pop$pop[1], removal_time, direction),
         call. = FALSE)
  }
}


# Calculate overlap between subsequent spatial maps
compute_overlaps <- function(x) {
  sf::st_agr(x) <- "constant"
  sapply(
    seq_len(nrow(x))[-1], function(i) {
      a <- x[i - 1, ]
      b <- x[i, ]
      intersection <- sf::st_intersection(a, b)
      if (nrow(intersection) == 0) return(0)
      sf::st_area(intersection) / sf::st_area(b)
    }
  )
}


# Take care of missing interactions and offspring distances
set_distances <- function(dispersal_table, resolution,
                          competition_dist, mate_dist, dispersal_dist) {
  if (is.null(competition_dist)) {
    if (all(is.na(dispersal_table$competition_dist))) {
      pop_names <- paste(unique(dispersal_table[is.na(dispersal_table$competition_dist), ]$pop), collapse = ", ")
      stop("Parameter 'competition_dist' missing for ", pop_names, " and a general
  value of this parameter was not provided to the compile() function", call. = FALSE)
    } else
      competition_dist <- utils::tail(dispersal_table$competition_dist[which(!is.na(dispersal_table$competition_dist))], 1)
  }
  # replace all NA values with the last specified competition distance
  dispersal_table$competition_dist[is.na(dispersal_table$competition_dist)] <- competition_dist

  if (is.null(mate_dist)) {
    if (all(is.na(dispersal_table$mate_dist))) {
      pop_names <- paste(unique(dispersal_table[is.na(dispersal_table$mate_dist), ]$pop), collapse = ", ")
      stop("Parameter 'mate_dist' missing for ", pop_names, " and a general
    value of this parameter was not provided to the compile() function", call. = FALSE)
    } else
      mate_dist <- utils::tail(dispersal_table$mate_dist[which(!is.na(dispersal_table$mate_dist))], 1)
  }
  # replace all NA values with the last specified mate choice distance
  dispersal_table$mate_dist[is.na(dispersal_table$mate_dist)] <- mate_dist

  if (is.null(dispersal_dist)) {
    if (all(is.na(dispersal_table$dispersal_dist))) {
      pop_names <- paste(unique(dispersal_table[is.na(dispersal_table$dispersal_dist), ]$pop), collapse = ", ")
      stop("Parameter 'dispersal_dist' missing for ", pop_names, " and a general
    value of this parameter was not provided to the compile() function", call. = FALSE)
    } else
      dispersal_dist <- utils::tail(dispersal_table$dispersal_dist[which(!is.na(dispersal_table$dispersal_dist))], 1)
  }
  # replace all NA values with the last specified dispersalchoice distance
  dispersal_table$dispersal_dist[is.na(dispersal_table$dispersal_dist)] <- dispersal_dist

  dispersal_table[, c("competition_dist", "mate_dist", "dispersal_dist")] <-
    dispersal_table[, c("competition_dist", "mate_dist", "dispersal_dist")] / resolution

  dispersal_table
}

# Return the map attribute of a slendr object
get_map <- function(x) {
  if (!inherits(x, "slendr"))
    stop("Can't access a map attribute of a non-slendr type object", call. = FALSE)

  map <- attr(x, "map")
  if (inherits(map, "slendr_map"))
    map
  else
    NULL
}

# Does a given object have an attribute map?
has_map <- function(x) {
  inherits(get_map(x), "slendr_map")
}


# Process the sampling schedue
process_sampling <- function(samples, model, verbose = FALSE) {
  # if no explicit sampling schedule was given, try to generate it at least
  # for the populations which survive to the present
  if (is.null(samples)) {
    if (verbose)
      message("Tree-sequence recording is on but no sampling schedule was given. ",
              "Generating one for all individuals surviving to the end of the simulation.")
    surviving_pops <- purrr::keep(model$populations, ~ attr(.x, "remove") == -1)

    # generate sampling schedule, remembering all individuals from all populations
    # surviving to the end of the simulation (which will happen at `end_time`)
    start_times <- purrr::map_int(model$populations, ~ attr(.x, "history")[[1]]$time)
    if (model$direction == "backward") {
      start <- max(start_times)
      end_time <- start - model$orig_length
    } else {
      start <- min(start_times)
      end_time <- start + model$orig_length
    }
    samples <- do.call(
      sampling,
      c(list(model = model, times = end_time), purrr::map(surviving_pops, ~ list(.x, Inf)))
    )
    if (is.null(samples)) {
      warning("No populations survive to the end of the simulations which means ",
              "that no individuals will be remembered.", call. = FALSE)
      return(NULL)
    } else
      samples$x <- samples$y <- samples$x_orig <- samples$y_orig <- NA
  }

  # in case a backwards time model is not to be simulated all the way to the
  # present (i.e. time 0), the conversion of sampling times from absolute model
  # time units into SLiM's forward time units needs to be adjusted relative
  # to the actual start of the population (the oldest split in the model), not
  # to the `orig_length` which would be the duration time of the simulation
  # (if not, `convert_to_forward` would translate into negative SLiM generations)
  if (model$direction == "backward" && model$orig_length != get_oldest_time(model$populations))
    end_time <- get_oldest_time(model$populations)
  else
    end_time <- model$orig_length

  df <- dplyr::group_by(samples, time, pop, x, y, x_orig, y_orig) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    dplyr::arrange(time) %>%
    convert_to_forward(
      direction = model$direction,
      columns = "time",
      generation_time = model$generation_time,
      end_time = end_time
    ) %>%
    dplyr::arrange(time_gen) %>%
    dplyr::select(pop, n, time_gen, x, y, time_orig, x_orig, y_orig)

  # if locations are missing, replace NA with -1 values for SLiM to understand
  df <- replace(df, is.na(df), -1)

  df <- df %>% dplyr::mutate(n = ifelse(is.infinite(n), "INF", n))

  df
}

# Make sure all given locations fall within world bounding box
check_location_bounds <- function(locations, map) {
  xrange <- attr(map, "xrange")
  yrange <- attr(map, "yrange")

  checks <- sapply(locations, function(loc) {
    loc[1] >= xrange[1] & loc[1] <= xrange[2] &
    loc[2] >= yrange[1] & loc[2] <= yrange[2]
  })

  if (!all(checks))
    stop("The following locations fall outside of the world map: ",
         paste(locations[!checks], collapse = ", "), call. = FALSE)
}

# Convert SLiM time units as they are saved in the tree-sequence output
# (and also other slendr output formats such as the locations of individuals
# or ancestry proportions over time) back to user-specified time units
# (either forward or backward)
convert_slim_time <- function(times, model) {
  ancestors <- dplyr::filter(model$splits, parent == "ancestor")

  if (model$direction == "backward") {
    result <- times * model$generation_time
    # does the backward simulation model terminate sooner than "present-day"?
    # if so, shift the times to start at the original time specified by user
    if (max(ancestors[, ]$tsplit_orig) != model$orig_length)
      result <- result + (ancestors[, ]$tsplit_orig - model$orig_length)
  } else {
    result <- (model$length - times + 1) * model$generation_time
    # did the simulation start at a later time than "generation 1"?
    # if it did, shift the time appropriately

    if (min(round(ancestors[, ]$tsplit_orig / model$generation_time) != 1))
      result <- result + ancestors[1, ]$tsplit_orig - model$generation_time
  }
  as.numeric(result)
}

# Convert msprime node time units into the user-specified time units
convert_msprime_time <- function(time, model) {
  if (model$direction == "forward")
    as.numeric(model$orig_length - (time - 1) * model$generation_time)
  else {
    as.numeric(time * model$generation_time)
  }
}

get_oldest_time <- function(populations) {
  max(sapply(populations, function(pop) attr(pop, "history")[[1]]$time))
}

kernel_fun <- function(fun = c("normal", "uniform", "cauchy", "exponential")) {
  match.arg(fun)
}

ask_install <- function(module) {
  answer <- utils::menu(c("No", "Yes"),
                        title = paste("Python module", module,
                                      "is missing in the environment. Install?"))
  answer == 2
}

check_env_present <- function() {
  tryCatch({
      "automatic_slendr_python_env" %in% reticulate::conda_list()$name
  }, error = function(cond) FALSE
  )
}
 
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

utils::globalVariables(
  names = c(
    ".", "node_id", "location", "name", "level", "child_id", "child_time",
    "parent_id", "parent_time", "child_pop", "parent_pop", "child_location",
    "parent_location", "connection", "left", "right", "parent", "tsplit_gen",
    "tstart_orig", "tend_orig", "time_gen", "n", "pop", "time_orig", "time",
    "pop_is", "time.x", "time.y", "ind_id", "remembered", "retained", "alive",
    "pedigree_id", "from_x", "from_y", "from", "to_x", "to_y", "to",
    "node1.name", "node2.name", "type", "label", "rate", "pop_id", "vcf_file",
    "gen", "newx", "newy", "child", "time", "node_label", "chr_name", "pos",
    "pyslim", "tskit", "msprime", "x", "y", "x_orig", "y_orig",
    "orig_x", "orig_y", "phylo_id"
  ), package = "slendr"
)
