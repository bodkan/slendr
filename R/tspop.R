#' Extract ancestry tracts
#' Extract ancestry tracts in sampled chromosomes (EXPERIMENTAL)
#'
#' See <https://tspop.readthedocs.io/en/latest/basicusage.html> for more
#' details.
#'
#' @param ts Tree sequence object of the class \code{slendr_ts}
#' @param census Census time (in slendr time units)
#' @param squashed Should ancestry tracts be squashed?
#' @param source Source population to filter ancestry tracts for
#' @export
ts_tracts <- function(ts, census, squashed = TRUE, source = NULL) {
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
    nodes <- ts_nodes(ts)

    tracts$source_pop <- model$splits$pop[tracts$population + 1]
    tracts$source_pop_id <- tracts$population
    tracts$population <- NULL
    pop_factors <- order_pops(model$populations, model$direction)
    tracts$source_pop <- factor(tracts$source_pop, levels = pop_factors)

    # tracts$name <- vapply(tracts$sample, function(node_id) nodes[nodes$node_id == node_id, ]$name,
    #                       FUN.VALUE = character(1))
    # tracts$node_id <- tracts$sample
    tracts <- dplyr::inner_join(
      tracts,
      nodes %>% dplyr::filter(sampled) %>% dplyr::select(name, node_id, pop),
      by = c("sample" = "node_id")
    )
    tracts$node_id <- tracts$sample
    tracts$sample <- NULL
  }

  if (!is.null(source)) {
    if (all(is.character(source))) {
      tracts <- tracts[tracts$source_pop %in% source, ]
    } else if (all(is.integer(source))) {
      tracts <- tracts[tracts$source_id %in% source, ]
    } else {
      stop("When providing multiple ancestry sources, all must be of the same type\n",
           "(either a vector of population names or integer IDs).",
           call. = FALSE)
    }
  }

  tracts <-
    dplyr::select(tracts, name, pop, source_pop, left, right, dplyr::everything()) %>%
    dplyr::mutate(length = right - left)

  tracts
}
