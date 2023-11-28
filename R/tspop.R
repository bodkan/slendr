#' Extract ancestry tracts from the tree sequence (EXPERIMENTAL)
#'
#' See <https://tspop.readthedocs.io/en/latest/basicusage.html> for more
#' details.
#'
#' @param ts Tree sequence object of the class \code{slendr_ts}
#' @param census Census time (in slendr time units)
#' @param squashed Should ancestry tracts be squashed (i.e., should continuous
#'   tracts that can be traced to different ancestral nodes be merged)? Default
#'   is \code{TRUE}.
#'
#' @returns A data frame containing coordinates of ancestry tracts
#'
#' @export
ts_tracts <- function(ts, census, squashed = TRUE) {
  model <- attr(ts, "model")
  from_slendr <- !is.null(model)

  # slendr tree sequences assume census times given in slendr-based time units
  # (whatever those might be depending on the user model)
  if (from_slendr) {
    if (attr(ts, "type") == "SLiM")
      stop("Extracting ancestry tracts is only supported for non-SLiM tree sequences",
           call. = FALSE)

    if (!census %in% model$geneflow$tstart_orig)
      stop("Census time ", census, " is not among gene-flow times in the model", call. = FALSE)

    which_gf <- model$geneflow$tstart_orig == census
    # get backwards-time generations-based time of gene flow in script.py
    census_gen <- model$length - model$geneflow$tstart_gen[which_gf] + 1
  } else {
    # non-slendr tree sequences naturally expect census time to be in tskit
    # time units
    census_gen <- census
  }

  pa <- tspop$get_pop_ancestry(ts, census_gen)
  # print out the summary right away
  summary(pa)
  cat("\n")

  if (squashed)
    tracts <- pa$squashed_table
  else
    tracts <- pa$ancestry_table

  tracts <- dplyr::as_tibble(tracts)

  if (from_slendr) {
    # add symbolic names of individuals and their populations to each tract by
    # joining with the annotated nodes table based on the node ID
    samples <- ts_nodes(ts) %>% dplyr::filter(sampled) %>% dplyr::select(name, node_id, pop)
    tracts <- dplyr::inner_join(tracts, samples, by = c("sample" = "node_id"))

    # add symbolic names of the source populations
    pop_factors <- order_pops(model$populations, model$direction)
    tracts$source_pop <- factor(model$splits$pop[tracts$population + 1], levels = pop_factors)

    # rename the population/sample/ancestor ID columns to match other slendr functions
    tracts$source_pop_id <- tracts$population
    tracts$node_id <- tracts$sample
    # and remove the original names
    tracts$population <- NULL
    tracts$sample <- NULL

    # non-squashed tract table contains an extra column, so take care of it
    if (!squashed) {
      tracts$ancestor_id <- tracts$ancestor
      tracts$ancestor <- NULL
      ancestor_col <- "ancestor_id"
    } else
      ancestor_col <- NULL

    columns <- c("name", "node_id", "pop", "source_pop", "left", "right", "length",
                 ancestor_col, "source_pop_id")
  }

  # add a length of each tract and reorder columns, for convenience
  tracts <- tracts %>% dplyr::mutate(length = right - left) %>% .[, columns]

  tracts
}
