#' @export
expand <- function(pop, by, end, start, overlap = 0.8, snapshots = NULL,
                   polygon = NULL, lock = FALSE, verbose = TRUE) {
  .Deprecated("expand_range")
  expand_range(pop, by, end, start, overlap, snapshots = NULL,
               polygon = NULL, lock, verbose)
}

#' @export
shrink <- function(pop, by, end, start, overlap = 0.8, snapshots = NULL,
                   polygon = NULL, lock = FALSE, verbose = TRUE) {
  .Deprecated("shrink_range")
  expand_range(pop, by, end, start, overlap, snapshots = NULL,
               polygon = NULL, lock, verbose)
}

#' @export
plot_graph <- function(model) .Deprecated("plot_model")

#' @export
explore <- function(model) {
  .Deprecated("explore_model")
  explore_model(model)
}

#' @export
compile <- function(populations, generation_time, path = NULL, resolution = NULL,
                    competition_dist = NULL, mate_dist = NULL, dispersal_dist = NULL,
                    geneflow = list(), overwrite = FALSE,
                    sim_length = NULL, direction = NULL,
                    slim_script = system.file("scripts", "script.slim", package = "slendr"),
                    description = "", dir = NULL) {
  .Deprecated("compile_model")
  compile_model(
    populations, generation_time, path, resolution, competition_dist,
    mate_dist, dispersal_dist, geneflow, overwrite, sim_length, direction,
    slim_script, description, dir
  )
}

#' @export
sampling <- function(model, times, ..., locations = NULL, strict = FALSE) {
  .Deprecated("schedule_sampling")
  schedule_sampling(model, times, ..., locations, strict)
}

#' @export
read <- function(path) {
  .Deprecated("read_model")
  read_model(path)
}

#' @export
dispersal <- function(pop, time, competition_dist = NA, mate_dist = NA, dispersal_dist = NA,
                      dispersal_fun = NULL) {
  .Deprecated("set_dispersal")
  set_dispersal(pop, time, competition_dist, mate_dist, dispersal_dist, dispersal_fun)
}

#' @export
boundary <- function(pop, time, center = NULL, radius = NULL, polygon = NULL, lock = FALSE) {
  .Deprecated("set_boundary")
  set_boundary(pop, time, center, radius, polygon, lock)
}

#' @export
geneflow <- function(from, to, rate, start, end, overlap = TRUE) {
  .Deprecated("gene_flow")
  gene_flow(from, to, rate, start, end, overlap)
}

#' @export
plot.slendr <- function(..., time = NULL, geneflows = FALSE,
                        graticules = "original",
                        intersect = TRUE, show_map = TRUE,
                        title = NULL, interpolated_maps = NULL) {
  .Deprecated("plot_map")
}
