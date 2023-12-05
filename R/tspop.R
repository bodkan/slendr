#' Extract ancestry tracts from a tree sequence (EXPERIMENTAL)
#'
#' Extract a data frame with coordinates of ancestry tracts from a given tree sequence.
#'
#' This functions implements an R-friendly interface to an algorithm for
#' extracting ancestry tracts provided by the Python module tspop
#' <https://tspop.readthedocs.io/en/latest/> and described by Georgia Tsambos.
#' Please make sure to cite their paper which describes the algorithm in
#' detail: <https://academic.oup.com/bioinformaticsadvances/article/3/1/vbad163/7429395>.
#' For more technical details, see also the tutorial at:
#' <https://tspop.readthedocs.io/en/latest/basicusage.html>.
#'
#' In general, when using this function on a slendr-generated tree sequence,
#' please be aware that the output changes slightly to what you would get by
#' running the pure \code{tspop.get_pop_ancestry()} in Python. First,
#' \code{ts_tracts()} populates the output data frame with additional metadata
#' (such as names of individuals or populations). Additionally, for slendr models,
#' it is specifically designed to only return ancestry tracts originating to a
#' an ancestral population which contributed its ancestry during a gene-flow
#' event which started at a specific time (i.e., scheduled in a model via
#' the \code{gene_flow()}) function. It does not return every single ancestry
#' tracts present in the tree sequence for every single sample node (and every
#' single potential ancestry population) as does the \code{tspop.get_pop_ancestry()}
#' Python method.
#'
#' That said, when run on a tree sequence which does not originate from a slendr
#' simulation, the behavior of \code{ts_tracts()} is identical to that of the
#' underlying \code{tspop.get_pop_ancestry()}.
#'
#' As of the current version of slendr, \code{ts_tracts()} only works for
#' slendr/msprime sequences but not on slendr/SLiM tree sequences. Support for
#' slendr-generated SLiM tree sequences is in development. Tree sequences from
#' non-slendr msprime and SLiM tree sequences is not restricted in any way and,
#' as mentioned in the previous paragraph, \code{ts_tracts()} on such tree
#' sequences effectively reduces to the standard \code{tspop.get_pop_ancestry()}
#' call.
#'
#' @param ts Tree sequence object of the class \code{slendr_ts}
#' @param census Census time. See the documentation linked in the Details for more
#'   information. If a slendr-specific tree sequence was provided as \code{ts},
#'   the census time is expected to be given in slendr model-specific time units,
#'   and must correspond to some gene-flow event encoded by the model.
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

  # populate the raw tspop data frame with slendr-specific symbolic names for
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
