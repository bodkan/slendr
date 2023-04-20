#' Check that all dependencies are available for slendr examples
#'
#' @param python Is the slendr Python environment required?
#' @param slim Is SLiM required?
#'
#' @return No return value. Called only to result in an error message if a
#'   particular software dependency is missing for an example to run.
#'
#' @export
#' @keywords internal
check_dependencies <- function(python = FALSE, slim = FALSE) {
  # check whether SLiM and Python are present (only if needed!)
  missing_slim <- if (slim) !all(Sys.which("slim") != "") else FALSE
  missing_python <- if (python) !is_slendr_env_present() else FALSE

  if (missing_slim | missing_python) {
    if (interactive()) {
      error_slim <- if (missing_slim) "SLiM" else ""
      error_python <- if (missing_python) "slendr Python environment" else ""
      stop(sprintf("Missing requirements for this example: %s",
                   paste(error_slim, error_python, sep = ", ")), call. = FALSE)
    } else
      q()
  }
}

# Internal implementation of expand_range() and shrink_range() functions
shrink_or_expand <- function(pop, by, end, start, overlap, snapshots, polygon,
                             lock, verbose) {
  check_event_time(c(start, end), pop)
  check_removal_time(start, pop)
  check_removal_time(end, pop)

  map <- attr(pop, "map")

  # get the last active population size
  prev_N <- sapply(attr(pop, "history"), function(event) event$N) %>%
    Filter(Negate(is.null), .) %>%
    unlist %>%
    utils::tail(1)

  # get the last available population boundary
  region_start <- pop[nrow(pop), ]
  sf::st_agr(region_start) <- "constant"

  if (is.null(snapshots)) {
    n <- 1
    message("Iterative search for the minimum sufficient number of intermediate
spatial snapshots, starting at ", n, ". This should only take a couple of
seconds, but if you don't want to wait, you can set `snapshots = N` manually.")
  } else
    n <- snapshots

  # iterate through the number of intermediate spatial boundaries to reach
  # the required overlap between subsequent spatial maps
  repeat {
    times <- seq(start, end, length.out = n + 1)[-1]

    # generate intermediate spatial maps, starting from the last one
    inter_regions <- list()
    inter_regions[[1]] <- region_start
    for (i in seq_along(times)) {
      exp_region <- sf::st_buffer(inter_regions[[1]], dist = i * (by / n))
      exp_region$time <- times[i]
      sf::st_agr(exp_region) <- "constant"

      # restrict the next spatial boundary to the region of interest
      if (!is.null(polygon)) {
        if (!inherits(polygon, "slendr_region"))
          polygon <- region(polygon = polygon, map = map)
        exp_region <- sf::st_intersection(exp_region, polygon)
        exp_region$region <- NULL
        sf::st_agr(exp_region) <- "constant"
      }

      inter_regions[[i + 1]] <- exp_region
    }

    if (!is.null(snapshots)) break

    # if the boundary is supposed to be shrinking, the order of spatial maps
    # must be reversed in order to check the amount of overlap
    direction <- ifelse(by < 0, rev, identity)
    overlaps <- compute_overlaps(do.call(rbind, direction(inter_regions)))

    if (all(overlaps >= overlap)) {
      message("The required ", sprintf("%.1f%%", 100 * overlap),
              " overlap between subsequent spatial maps has been met")
      break
    } else {
      n <- n + 1
      if (verbose)
        message("- Increasing to ", n, " snapshots")
    }
  }

  all_maps <- do.call(rbind, inter_regions[-1]) %>% rbind(pop, .)
  sf::st_agr(all_maps) <- "constant"

  result <- copy_attributes(
    all_maps, pop,
    c("map", "parent", "remove", "intersect", "aquatic", "history")
  )

  start_area <- sf::st_area(utils::head(inter_regions, 1)[[1]])
  end_area <- sf::st_area(utils::tail(inter_regions, 1)[[1]])
  action <- ifelse(start_area < end_area, "expand", "contract")

  attr(result, "history") <- append(attr(result, "history"), list(data.frame(
    pop =  unique(region_start$pop),
    event = action,
    tstart = start,
    tend = end
  )))


  if (lock) {
    areas <- as.numeric(sapply(inter_regions, sf::st_area))
    area_changes <- areas[-1] / areas[-length(areas)]
    new_N <- as.integer(round(cumprod(area_changes) * prev_N))
    prev_N <- c(prev_N, new_N[-length(new_N)])
    times <- sapply(inter_regions[-1], `[[`, "time")
    changes <- lapply(seq_len(length(new_N)), function(i) {
      data.frame(
        pop =  unique(pop$pop),
        event = "resize",
        how = "step",
        N = new_N[i],
        prev_N = prev_N[i],
        tresize = times[i],
        tend = NA
      )
    })
    attr(result, "history") <- append(attr(result, "history"), changes)
    # for (i in seq_along(inter_regions)[-1]) {
    #   time <- inter_regions[[i]]$time
    #   result <- resize(result, N = new_N[i - 1], time = time, how = "step")
    # }
  }

  result
}

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

  direction <- time_direction(model)
  if (direction == "forward") {
    before_op <- `<=`
    after_op <- `>=`
  } else {
    before_op <- `>=`
    after_op <- `<=`
  }

  if (!is.null(time))
    geneflows <- subset(model$geneflow, before_op(tstart_orig, time) & after_op(tend_orig, time))
  else
    geneflows <- model$geneflow

  geneflows$from <- factor(geneflows$from, levels = pop_names)
  geneflows$to <- factor(geneflows$to, levels = pop_names)

  migr_coords <- lapply(seq_len(nrow(geneflows)), function(row_i) {

    # if time point was not provided, simply take the midpoint of the current
    # gene-flow event
    if (is.null(time)) {
      time <- geneflows[row_i, c("tstart_orig", "tend_orig")] %>% as.numeric() %>% mean()
    }

    from <- model$populations[pop_names == geneflows[row_i, ]$from][[1]] %>%
      .[before_op(.$time, time), ] %>%
      .[nrow(.), ]

    to <- model$populations[pop_names == geneflows[row_i, ]$to][[1]] %>%
      .[before_op(.$time, time), ] %>%
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


# Get direction of time implied by the history of the population
#
# @param x Object of the class \code{slendr_pop} or \code{slendr_model}
#
# @return Either "forward", "backward", or "unknown"
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
# (used exclusively in the schedule_sampling() function to avoid situations when
# a user would sample from a population in the same generation that it
# would be created)
check_present_time <- function(time, pop, offset, direction = NULL, allow_same = FALSE) {
  if (is.null(direction))
    direction <- time_direction(pop)
  split_time <- get_lineage_splits(pop)[1]

  if ((!allow_same && time == split_time) ||
      (direction == "backward" && time > split_time - offset) ||
      (direction == "forward" && time < split_time + offset))
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
                          competition, mating, dispersal) {
  if (is.null(competition)) {
    if (all(is.na(dispersal_table$competition))) {
      pop_names <- paste(unique(dispersal_table[is.na(dispersal_table$competition), ]$pop), collapse = ", ")
      stop("Parameter 'competition' missing for ", pop_names, " and a general
  value of this parameter was not provided to the compile() function", call. = FALSE)
    } else
      competition <- utils::tail(dispersal_table$competition[which(!is.na(dispersal_table$competition))], 1)
  }
  # replace all NA values with the last specified competition distance
  dispersal_table$competition[is.na(dispersal_table$competition)] <- competition

  if (is.null(mating)) {
    if (all(is.na(dispersal_table$mating))) {
      pop_names <- paste(unique(dispersal_table[is.na(dispersal_table$mating), ]$pop), collapse = ", ")
      stop("Parameter 'mating' missing for ", pop_names, " and a general
    value of this parameter was not provided to the compile() function", call. = FALSE)
    } else
      mating <- utils::tail(dispersal_table$mating[which(!is.na(dispersal_table$mating))], 1)
  }
  # replace all NA values with the last specified mate choice distance
  dispersal_table$mating[is.na(dispersal_table$mating)] <- mating

  if (is.null(dispersal)) {
    if (all(is.na(dispersal_table$dispersal))) {
      pop_names <- paste(unique(dispersal_table[is.na(dispersal_table$dispersal), ]$pop), collapse = ", ")
      stop("Parameter 'dispersal' missing for ", pop_names, " and a general
    value of this parameter was not provided to the compile() function", call. = FALSE)
    } else
      dispersal <- utils::tail(dispersal_table$dispersal[which(!is.na(dispersal_table$dispersal))], 1)
  }
  # replace all NA values with the last specified dispersalchoice distance
  dispersal_table$dispersal[is.na(dispersal_table$dispersal)] <- dispersal

  dispersal_table[, c("competition", "mating", "dispersal")] <-
    dispersal_table[, c("competition", "mating", "dispersal")] / resolution

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


# Process the sampling schedule which was provided by the user or -- if no
# sampling schedule was specified -- generate one automatically for populations
# which survive to the end of the simulation.
process_sampling <- function(samples, model, verbose = FALSE) {
  # if no explicit sampling schedule was given, try to generate it at least
  # for the populations which survive to the present
  if (is.null(samples)) {
    if (verbose)
      message("Tree-sequence recording is on but no sampling schedule was given. ",
              "Generating one for all individuals surviving to the end of the simulation.")

    # get a list of populations surviving to the end of the simulation
    surviving_pops <- purrr::keep(model$populations, ~ attr(.x, "remove") == -1)

    # find out at which time is the simulation supposed to end ...
    start_times <- purrr::map_int(model$populations,
                                  ~ attr(.x, "history")[[1]]$time)
    if (model$direction == "backward") {
      start_time <- max(start_times)
      # modulo for length isn't divisible by generation time
      sampling_time <- start_time - model$orig_length +
        model$orig_length %% model$generation_time
    } else {
      start_time <- min(start_times)
      sampling_time <- start_time + model$generation_time * round(model$orig_length / model$generation_time)
    }

    # ... and then call
    #    schedule_sampling(model, end_time, <all surviving populations>)
    # which is what would be called normally by the user, manually
    samples <- do.call(
      schedule_sampling,
      c(list(model = model, times = sampling_time),
        purrr::map(surviving_pops, ~ list(.x, Inf)))
    )

    # take care of the edge in which no populations are expected to survive
    # until the end of the simulation, leaving no samples to be remembered
    # TODO: is this actually allowed to happen in SLiM?
    if (is.null(samples)) {
      warning("No populations survive to the end of the simulations which means ",
              "that no individuals will be remembered.", call. = FALSE)
      return(NULL)
    } else {
      # default sampling schedules are not spatial
      samples$x <- samples$y <- samples$x_orig <- samples$y_orig <- NA
    }
  }

  # sum up all individual `n` counts for each pop/time/location in case of
  # multiples
  df <- dplyr::group_by(samples, time, pop, x, y, x_orig, y_orig) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    dplyr::arrange(time)

  # in case a backwards time model is not to be simulated all the way to the
  # present (i.e. time 0), the conversion of sampling times from absolute model
  # time units into SLiM's forward time units needs to be adjusted relative
  # to the actual start of the population (the oldest split in the model), not
  # to the `orig_length` which would be the duration time of the simulation
  # (if not, `convert_to_forward` would translate into negative SLiM generations)
  oldest_time <- get_oldest_time(model$populations, model$direction)
  if (model$direction == "backward" && oldest_time != model$orig_length) {
    end_time <- oldest_time
  } else if (model$direction == "forward" && oldest_time > 1) {
    # same for forward models starting not in generation 1
    time_orig <- df$time
    df$time <- df$time - oldest_time + model$generation_time
    end_time <- model$orig_length
  } else
    end_time <- model$orig_length

  processed_schedule <- df %>%
    convert_to_forward(
      direction = model$direction,
      columns = "time",
      generation_time = model$generation_time,
      end_time = end_time
    ) %>%
    dplyr::arrange(time_gen) %>%
    dplyr::select(pop, n, time_gen, x, y, time_orig, x_orig, y_orig)

  # if the original times should be replaced, do it
  if (model$direction == "forward" && oldest_time > 1)
    processed_schedule$time_orig <- time_orig

  # if locations are missing, replace NA with -1 values for SLiM to understand
  processed_schedule <- replace(processed_schedule, is.na(processed_schedule), -1)

  processed_schedule %>% dplyr::mutate(n = ifelse(is.infinite(n), "INF", n))
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
  ancestors <- dplyr::filter(model$splits, parent == "__pop_is_ancestor")

  if (model$direction == "backward") {
    oldest_time <- max(ancestors[, ]$tsplit_orig)

    result <- times * model$generation_time +
      (max(ancestors[, ]$tsplit_orig) - model$orig_length)

    # does the backward simulation model terminate sooner than "present-day"? if
    # so, shift the times to start at the original time specified by user (also
    # check for the situation where the simulation wouldn't end at 0 because the
    # length of the simulation is not divisible by generation time)
    shortened <- oldest_time != model$orig_length
    indivisible <- model$orig_length %% model$generation_time != 0
    if (shortened && !indivisible) {
      result <- result + model$orig_length %% model$generation_time
    } else if (indivisible) {
      result <- result + model$orig_length -
        round(model$orig_length / model$generation_time) * model$generation_time
    }
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

get_oldest_time <- function(populations, direction) {
  times <- sapply(populations, function(pop) attr(pop, "history")[[1]]$time)
  if (direction == "forward")
    min(times)
  else
    max(times)
}

kernel_fun <- function(fun = c("normal", "uniform", "cauchy", "exponential",
                               "brownian")) {
  match.arg(fun)
}

ask_install <- function(module) {
  answer <- utils::menu(c("No", "Yes"),
                        title = paste("Python module", module,
                                      "is missing in the environment. Install?"))
  answer == 2
}

is_slendr_env_present <- function() {
  tryCatch({
    PYTHON_ENV %in% reticulate::conda_list()$name
  }, error = function(cond) FALSE
  )
}

order_pops <- function(populations, direction) {
  pop_names <- purrr::map_chr(populations, ~ .x$pop[1])
  split_times <- purrr::map_int(populations, ~ attr(.x, "history")[[1]]$time)
  names(split_times) <- pop_names
  if (length(direction) > 0 && direction == "backward") {
    split_times <- sort(split_times, decreasing = TRUE)
  } else if (length(direction) > 0 && direction == "forward") {
    split_times <- sort(split_times)
  }
  names(split_times)
}

#' Pipe operator
#'
#' @return See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
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
    "pop_is", "time.x", "time.y", "ind_id", "sampled", "remembered", "retained",
    "alive", "pedigree_id", "from_x", "from_y", "from", "to_x", "to_y", "to",
    "node1.name", "node2.name", "type", "label", "rate", "pop_id", "vcf_file",
    "gen", "newx", "newy", "child", "time", "node_label", "chr_name", "pos",
    "pyslim", "tskit", "msprime", "x", "y", "x_orig", "y_orig",
    "orig_x", "orig_y", "phylo_id", "raster_x", "raster_y",
    "pop.y", "pop_id.y", "time_tskit", "time_tskit.x", "time_tskit.y",
    "N", "center", "child_node_id", "child_phylo_id", "geometry", "parent_node_id",
    "parent_phylo_id", "set_boundary", "xend", "xmax", "xmin", "yend",
    "arc_degree"
  ), package = "slendr"
)
