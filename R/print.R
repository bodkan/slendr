#' Print a short summary of a \code{slendr} object
#'
#' All spatial objects in the slendr package are internally represented as
#' Simple Features (\code{sf}) objects. This fact is hidden in most
#' circumstances this, as the goal of the slendr package is to provide
#' functionality at a much higher level (population boundaries, geographic
#' regions, instead of individual polygons and other "low-level" geometric
#' objects), without the users having to worry about low-level details involved
#' in handling spatial geometries. However, the full \code{sf} object
#' representation can be always printed by calling \code{x[]}.
#'
#' @param x Object of a class \code{slendr} (either \code{slendr_pop},
#'   \code{slendr_map}, \code{slendr_region}, or \code{slendr_table})
#' @param ... Additional arguments passed to \code{print}
#'
#' @return No return value, used only for printing
#'
#' @export
print.slendr_pop <- function(x, ...) {
  print_header_info(x)

  cat("name:", unique(x$pop), "\n")

  if (has_map(x)) {
    # aquatic or terrestrial?
    if (attr(x, "aquatic") == FALSE)
      cat("habitat: terrestrial\n")
    else
      cat("habitat: aquatic\n")

    cat("\nnumber of spatial maps:", nrow(x), "\n")
    print_map_info(x)
  } else
    cat("non-spatial population\n")

  # removal time of the population
  if (attr(x, "remove") == -1)
    cat("stays until the end of the simulation\n")
  else
    cat("scheduled removal at time ", attr(x, "remove"), "\n")

  print_pop_history(x)
}


#' @rdname print.slendr_pop
#' @export
print.slendr_region <- function(x, ...) {
  print_header_info(x)

  if (nrow(x))
    cat("name:", x$region, "\n\n")
  else
    cat("[this region object is empty]\n\n")

  print_map_info(x)
}


#' @rdname print.slendr_pop
#' @export
print.slendr_map <- function(x, ...) {
  print_header_info(x)
  print_map_info(x)
}


#' @rdname print.slendr_pop
#' @export
print.slendr_model <- function(x, ...) {
  print_header_info(x)

  cat("populations:", paste0(x$splits$pop, collapse = ", "), "\n")
  cat("geneflow events: ")
  if (!is.null(x$geneflow))
    cat(nrow(x$geneflow), "\n")
  else
    cat("[no geneflow]\n")
  cat("generation time:", x$generation_time, "\n")
  cat("time direction:", x$direction, "\n")
  cat("total running length:", x$orig_length, "model time units\n")

  cat("model type: ")
  if (inherits(x$world, "slendr_map")) {
    cat("spatial\n")
    cat("  - number of spatial maps:", nrow(x$maps), "\n")
    cat("  - resolution:", x$resolution, "distance units per pixel\n\n")
  } else
    cat("non-spatial\n\n")

  cat("configuration files in:", normalizePath(x$path), "\n")
}


#' @rdname print.slendr_pop
#' @export
print.slendr_nodes <- function(x, ...) {
  model <- attr(x, "model")
  type <- attr(x, "type")

  from_slendr <- !is.null(model)

  sep <- print_header_info(x)

  if (from_slendr)
    direction <- model$direction
  else
    direction <- if (type == "SLiM") "forward" else "backward"

  cat("times are expressed in a", direction, "time direction\n")

  individuals <- as.data.frame(x) %>%
    dplyr::filter(!is.na(ind_id)) %>%
    dplyr::distinct(ind_id, .keep_all = TRUE)

  if (type == "SLiM") {
    cat("\nsummary of the table data contents:\n")

    sampled <- individuals %>%
      dplyr::filter(sampled) %>%
      dplyr::group_by(pop) %>%
      dplyr::summarise(n = dplyr::n())

    remembered <- individuals %>%
      dplyr::filter(remembered) %>%
      dplyr::group_by(pop) %>%
      dplyr::summarise(n = dplyr::n())

    retained <- individuals %>%
      dplyr::filter(!sampled, !remembered, retained) %>%
      dplyr::group_by(pop) %>%
      dplyr::summarise(n = dplyr::n())

    alive <- individuals %>%
      dplyr::filter(alive) %>%
      dplyr::group_by(pop) %>%
      dplyr::summarise(n = dplyr::n())

    for (pop in unique(individuals$pop)) {
      n_sampled <- sampled[sampled$pop == pop, ]$n
      n_remembered <- remembered[remembered$pop == pop, ]$n
      n_retained <- retained[retained$pop == pop, ]$n
      n_alive <- alive[alive$pop == pop, ]$n
      cat(" ", pop, "-",
          ifelse(!length(n_sampled), 0, n_sampled), "'sampled',",
          ifelse(!length(n_remembered), 0, n_remembered), "'remembered',",
          ifelse(!length(n_retained), 0, n_alive), "'retained',",
          ifelse(!length(n_alive), 0, n_alive), "'alive' individuals\n")
    }

    cat("\ntotal:\n  - ")
    cat(sum(sampled$n), "'sampled' individuals\n  -",
        sum(remembered$n), "'remembered' individuals\n  -",
        sum(retained$n), "'retained' individuals\n  -",
        sum(alive$n), "'alive' individuals\n")
  } else if (nrow(individuals) > 0) {
    cat("\nsummary of the table data contents:\n")

    # dummy column for later printing of sampled individuals' times
    individuals$remembered <- TRUE
    populations <- if (is.null(model)) unique(individuals$pop) else model$splits$pop
    for (pop in populations) {
      n_sampled <- length(individuals[individuals$pop == pop, ]$ind_id)
      n_unsampled <- sum(is.na(individuals$ind_id))
      cat(" ", pop, "-", n_sampled, "'sampled',",
          n_unsampled, "'unsampled' individuals\n")
    }
    cat("\ntotal:", length(unique(individuals$ind_id)),
        "'sampled' individuals\n")
  }

  cat(sep)

  if (from_slendr)
    ts_direction <- model$direction
  else
    ts_direction <- "backward"

  if (nrow(individuals) > 0) {
    direction <- ifelse(ts_direction == "forward", "(counting from the start)", "'before present'")
    funs <- if (ts_direction == "forward") c(min, max) else c(max, min)
    individuals %>% dplyr::filter(sampled) %>% {
      cat("oldest sampled individual:", funs[[1]](.$time), "time units", direction, "\n")
      cat("youngest sampled individual:", funs[[2]](.$time), "time units", direction, "\n")
    }

    cat("\noldest node:", funs[[1]](x$time), "time units", direction, "\n")
    cat("youngest node:", funs[[2]](x$time), "time units", direction, "\n")

    cat(sep)
  }

  if (inherits(x, "sf"))
    cat("overview of the underlying sf object:\n\n")
  else
    cat("overview of the underlying table object:\n\n")

  dplyr::as_tibble(x) %>% print()
}


get_slendr_type <- function(x) {
  grep("slendr_", class(x), value = TRUE) %>%
    gsub("slendr_", "", .) %>%
    gsub("pop", "population", .)
}


print_header_info <- function(x) {
  type <- get_slendr_type(x)

  header <- sprintf("slendr '%s' object", type)
  sep <- paste(rep("-", nchar(header)), collapse = "")

  cat(header, "\n")
  cat(sep, "\n")

  return(paste(sep, "\n"))
}


print_map_info <- function(x) {
  type <- get_slendr_type(x)

  cat("map: ")
  if (type == "map" | has_map(x)) {
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
      cat("internal coordinate reference system", crs, "\n")
      units <- " (in degrees longitude and latitude)"
    }

    xrange <- attr(x, "xrange")
    yrange <- attr(x, "yrange")
    cat(sprintf("spatial limits%s:\n  - vertical %d ... %d\n  - horizontal %d ... %d\n",
                units, xrange[1], xrange[2], yrange[1], yrange[2]))
  } else
    cat("[no map defined]\n")
}

print_pop_history <- function(x) {
  cat("\npopulation history overview:\n")
  history <- attr(x, "history")

  first_event <- attr(x, "history")[[1]]
  prev_competition <- first_event$competition
  prev_mating <- first_event$mating
  prev_dispersal <- first_event$dispersal

  for (event in history) {
    if (nrow(event) > 1 && event$event == "resize" && event$how == "step") {
      # tstart <- base::round(event$tresize[1])
      # tend <- utils::tail(event$tresize, 1)
      sizes <- utils::tail(attr(x, "history"), 1)[[1]][1, c("N", "prev_N")]
      action <- ifelse(sizes$N < sizes$prev_N, "decrease", "increase")
      cat(sprintf("     [automatic %s from %d to %d individuals]\n",
                  action, event$prev_N[1], utils::tail(event$N, 1)))
    # population split
    } else if (event$event == "split") {
      cat("  - time ")
      cat(event$time, ": ", sep = "")
      parent <- attr(x, "parent")
      if (is.character(parent) && parent == "ancestor")
        cat("created as an ancestral population", sprintf("(N = %d)", event$N))
      else {
        cat("split from", parent$pop[1], sprintf("(N = %d)", event$N))
      }
    }

    # spatial dynamics events
    else if (event$event == "move") {
      cat(sprintf("  - time %d-%d: movement across a landscape", event$tstart, event$tend))
    } else if (event$event == "expand") {
      cat(sprintf("  - time %d-%d: range expansion", event$tstart, event$tend))
    } else if (event$event == "contract") {
      cat(sprintf("  - time %d-%d: range contraction", event$tstart, event$tend))
    } else if (event$event == "range") {
      cat(sprintf("  - time %d: change of the spatial boundary", event$time))
    }

    # population size change
    else if (event$event == "resize" && event$how == "step") {
      cat(sprintf("  - time %d: resize from %d to %d individuals",
                  event$tresize, event$prev_N, event$N))
    } else if (event$event == "resize" && event$how == "exponential") {
      cat(sprintf("  - time %d-%d: exponential resize from %d to %d individuals",
                  event$tresize, event$tend, event$prev_N, event$N))
    }

    # change of dispersal parameters
    else if (event$event == "dispersal") {
      cat(sprintf("  - time %d: change in spatial interaction", event$time))
      if (!is.na(event$competition) && event$competition != prev_competition) {
        cat("\n        - competition distance:", event$competition)
        prev_competition <- event$competition
      }
      if (!is.na(event$mating) && event$mating != prev_mating) {
        cat("\n        - mate choice distance:", event$mating)
        prev_mating <- event$mating
      }
      if (!is.na(event$dispersal) && event$dispersal != prev_dispersal) {
        cat("\n        - dispersal from parent:", event$dispersal)
        prev_dispersal <- event$dispersal
      }
    } else
      stop("Unknown event type", call. = FALSE)

    cat("\n")
  }
}
