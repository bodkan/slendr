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
#'   \code{slendr_map}, \code{slendr_region}, or \code{slendr_tsdata})
#' @param ... Additional arguments passed to \code{print}
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
  if (!is.null(x$geneflows))
    cat(nrow(x$geneflows), "\n")
  else
    cat("[no geneflow]\n")
  cat("generation time:", x$generation_time, "\n")
  cat("time direction:", x$direction, "\n")

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
print.slendr_tsdata <- function(x, ...) {
  model <- attr(x, "model")
  backend <- attr(x, "source")

  sep <- print_header_info(x)

  cat("data was extracted from a", backend, model$direction, "time model\n\n")

  cat("summary of the table data contents:\n")

  individuals <- as.data.frame(x) %>%
    dplyr::filter(!is.na(ind_id)) %>%
    dplyr::distinct(ind_id, .keep_all = TRUE)

  if (backend == "SLiM") {
    remembered <- individuals %>%
      dplyr::filter(remembered) %>%
      dplyr::group_by(pop) %>%
      dplyr::summarise(n = dplyr::n())

    retained <- individuals %>%
      dplyr::filter(!remembered, retained) %>%
      dplyr::group_by(pop) %>%
      dplyr::summarise(n = dplyr::n())

    n_other <- sum(is.na(x$ind_id))

    for (pop in model$splits$pop) {
      n_remembered <- remembered[remembered$pop == pop, ]$n
      n_retained <- retained[retained$pop == pop, ]$n
      cat(" ", pop, "-",
          ifelse(!length(n_remembered), 0, n_remembered), "'sampled',",
          ifelse(!length(n_retained), 0, n_retained), "'retained' individuals\n")
    }

    if (n_other == 0)
      node_str <- "no nodes"
    else if (n_other == 1)
      node_str <- "node"
    else
      node_str <- "nodes"

    cat("\ntotal:\n  -", sum(remembered$n), "'sampled' individuals\n  -",
        sum(retained$n), "'retained' individuals\n  -",
        n_other, node_str, "from 'recapitated' individuals\n")
  } else {
    # dummy column for later printing of sampled individuals' times
    individuals$remembered <- TRUE
    for (pop in model$splits$pop) {
      n_sampled <- length(individuals[individuals$pop == pop, ]$ind_id)
      n_unsampled <- sum(is.na(individuals$ind_id))
      cat(" ", pop, "-", n_sampled, "'sampled',",
          n_unsampled, "'unsampled' individuals\n")
    }
    cat("\ntotal:", length(individuals[!is.na(individuals$ind_id), ]),
        "'sampled' individuals")
  }

  cat(sep)

  direction <- ifelse(model$direction == "forward",
                      "(counting from the start)", "'before present'")
  funs <- if (model$direction == "forward") c(min, max) else c(max, min)
  individuals %>% dplyr::filter(remembered) %>% {
    cat("oldest sampled individual:", funs[[1]](.$time), "time units", direction, "\n")
    cat("youngest sampled individual:", funs[[2]](.$time), "time units", direction, "\n")
  }

  cat("\noldest node:", funs[[1]](x$time), "time units", direction, "\n")
  cat("youngest node:", funs[[2]](x$time), "time units", direction, "\n")

  cat(sep)

  if (!is.null(model$world) && backend == "SLiM")
    cat("overview of the underlying sf object:\n\n")
  else
    cat("overview of the underlying table object:\n\n")

  dplyr::as_tibble(x) %>%
    dplyr::arrange(is.na(name), -time, node_id) %>%
    print()
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
  prev_competition_dist <- first_event$competition_dist
  prev_mate_dist <- first_event$mate_dist
  prev_dispersal_dist <- first_event$dispersal_dist

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
        cat("created as an ancestral population")
      else {
        cat("split from", parent$pop[1])
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
      if (!is.na(event$competition_dist) && event$competition_dist != prev_competition_dist) {
        cat("\n        - competition distance:", event$competition_dist)
        prev_competition_dist <- event$competition_dist
      }
      if (!is.na(event$mate_dist) && event$mate_dist != prev_mate_dist) {
        cat("\n        - mate choice distance:", event$mate_dist)
        prev_mate_dist <- event$mate_dist
      }
      if (!is.na(event$dispersal_dist) && event$dispersal_dist != prev_dispersal_dist) {
        cat("\n        - dispersal from parent:", event$dispersal_dist)
        prev_dispersal_dist <- event$dispersal_dist
      }
    } else
      stop("Unknown event type", call. = FALSE)

    cat("\n")
  }
}
