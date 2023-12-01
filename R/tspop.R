#' Extract ancestry tracts from a tree sequence (EXPERIMENTAL)
#'
#' Extracts a table of ancestry tracts from a given tree sequence
#'
#' This functions implements an R-friendly interface to an algorithm for
#' extracting ancestry tracts provided by the Python module tspop
#' <https://tspop.readthedocs.io/en/latest/> and described by Georgia Tsambos.
#' Please make sure to cite their paper which describes the algorithm in
#' detail: <https://academic.oup.com/bioinformaticsadvances/article/3/1/vbad163/7429395>.
#'
#' For more technical details, see also the tutorial at:
#' <https://tspop.readthedocs.io/en/latest/basicusage.html>.
#'
#' @param ts Tree sequence object of the class \code{slendr_ts}
#' @param census Census time (in slendr time units) corresponding to some gene
#'   flow event
#' @param squashed Should ancestry tracts be squashed (i.e., should continuous
#'   tracts that can be traced to different ancestral nodes be merged)? Default
#'   is \code{TRUE}. If \code{FALSE}, these effectively continuous ancestry
#'   tracts will be split into individual segments, each assigned to a specific
#'   ancestral node ID (recorded in a column \code{ancestor_id}).
#' @param source From which source population to extract tracts for? if \code{NULL}
#'   (the default), ancestry tracts for all populations contributing gene flow
#'   at the census time will be reported. Otherwise, ancestry tracts from only
#'   specified source populations will be extracted. Note that this option is
#'   ignored for non-slendr tree sequences!
#'
#' @returns A data frame containing coordinates of ancestry tracts
#'
#' @export
ts_tracts <- function(ts, census, squashed = TRUE, source = NULL) {
  model <- attr(ts, "model")
  from_slendr <- !is.null(model)

  # again, as with many other tskit interface functions in slendr, make sure
  # the census time is correctly transformed from slendr-time context into
  # tskit's backward time units
  if (from_slendr) {
    if (attr(ts, "type") == "SLiM")
      stop("Extracting ancestry tracts is currently supported for non-SLiM tree sequences",
           call. = FALSE)

    # slendr tree sequences assume census times given in slendr-based time units
    # (whatever those might be depending on the user model)
      if (!census %in% model$geneflow$tstart_orig)
        stop("Census time ", census, " does not correspond to any gene-flow times", call. = FALSE)

    # identify which gene-flow event does the census time correspond to
    which_gf <- model$geneflow$tstart_orig == census
    # get backwards-time generations-based time of gene flow in script.py
    census_gen <- model$length - unique(model$geneflow$tstart_gen[which_gf]) + 1

    sources <- model$geneflow$from[which_gf]
    if (!is.null(source) && any(!source %in% sources))
      stop("These source(s) do not participate in the specified gene flow event: ",
           paste0(source[!source %in% sources], collapse = ", "), call. = FALSE)
    else
      source <- unique(model$geneflow$from[which_gf])
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

  # population the raw tspop data frame with slendr-specific symbolic names for
  # all columns where it makes sense, and rename columns to match other slendr's
  # tskit-related functions
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

    # filter for source populations of interest
    tracts <- tracts[tracts$source_pop %in% source, ]

    # add a length of each tract and reorder columns, for convenience
    tracts <- tracts %>% dplyr::mutate(length = right - left) %>% .[, columns]
  }

  tracts
}
