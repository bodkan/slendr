#' Deprecated function
#' @inheritParams expand_range
#' @export
expand <- function(pop, by, end, start, overlap = 0.8, snapshots = NULL,
                   polygon = NULL, lock = FALSE, verbose = TRUE) {
  .Deprecated("expand_range")
  expand_range(pop, by, end, start, overlap, snapshots = NULL,
               polygon = NULL, lock, verbose)
}

#' Deprecated function
#' @inheritParams shrink_range
#' @export
shrink <- function(pop, by, end, start, overlap = 0.8, snapshots = NULL,
                   lock = FALSE, verbose = TRUE) {
  .Deprecated("shrink_range")
  expand_range(pop, by, end, start, overlap, snapshots = NULL,
               polygon = NULL, lock, verbose)
}

#' Deprecated function
#' @inheritParams plot_model
#' @export
plot_graph <- function(model) .Deprecated("plot_model")

#' Deprecated function
#' @inheritParams explore_model
#' @export
explore <- function(model) {
  .Deprecated("explore_model")
  explore_model(model)
}

#' Deprecated function
#' @inheritParams compile_model
#' @export
compile <- function(populations, generation_time, path = NULL, resolution = NULL,
                    competition = NULL, mating = NULL, dispersal = NULL,
                    geneflow = list(), overwrite = FALSE,
                    sim_length = NULL, direction = NULL,
                    slim_script = system.file("scripts", "script.slim", package = "slendr"),
                    description = "", dir = NULL) {
  .Deprecated("compile_model")
  compile_model(
    populations, generation_time, path, resolution, competition,
    mating, dispersal, geneflow, overwrite, sim_length, direction,
    slim_script, description, dir
  )
}

#' Deprecated function
#' @inheritParams schedule_sampling
#' @export
sampling <- function(model, times, ..., locations = NULL, strict = FALSE) {
  .Deprecated("schedule_sampling")
  schedule_sampling(model, times, ..., locations, strict)
}

#' Deprecated function
#' @inheritParams read_model
#' @export
read <- function(path) {
  .Deprecated("read_model")
  read_model(path)
}

#' Deprecated function
#' @inheritParams set_dispersal
#' @export
dispersal <- function(pop, time, competition = NA, mating = NA, dispersal = NA,
                      dispersal_fun = NULL) {
  .Deprecated("set_dispersal")
  set_dispersal(pop, time, competition, mating, dispersal, dispersal_fun)
}

#' Deprecated function
#' @inheritParams set_range
#' @export
boundary <- function(pop, time, center = NULL, radius = NULL, polygon = NULL, lock = FALSE) {
  .Deprecated("set_range")
  set_range(pop, time, center, radius, polygon, lock)
}

#' Deprecated function
#' @inheritParams gene_flow
#' @export
geneflow <- function(from, to, rate, start, end, overlap = TRUE) {
  .Deprecated("gene_flow")
  gene_flow(from, to, rate, start, end, overlap)
}

#' Deprecated function
#' @inheritParams plot_map
#' @export
plot.slendr <- function(..., time = NULL, geneflows = FALSE,
                        graticules = "original",
                        intersect = TRUE, show_map = TRUE,
                        title = NULL, interpolated_maps = NULL) {
  .Deprecated("plot_map")
}
