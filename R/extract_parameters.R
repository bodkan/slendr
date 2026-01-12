#' Extract information from a compiled model or a simulated tree sequence
#'
#' This function extract a slendr model parameters used to compile a given model
#' object or simulate a tree sequence
#'
#' @param data Either an object of the class \code{slendr_ts} or \code{slendr_model}
#'
#' @returns A list of data frames containing parameters of the model used when
#'   compiling a model object
#'
#' @examples
#' \dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#' init_env()
#'
#' # load an example model and simulate a tree sequence from it
#' model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))
#' ts <- msprime(model, sequence_length = 1e5, recombination_rate = 0)
#'
#' # extract model parameters from a compiled model object as a list of data frames
#' extract_parameters(model)
#'
#' # the function can also extract parameters of a model which simulated a
#' # tree sequence
#' extract_parameters(ts)
#'
#' @export
extract_parameters <- function(data) {
  if (inherits(data, "slendr_ts")) {
    model <- attr(data, "model")
    if (is.null(model))
      stop("No slendr model configuration present in the tree sequence. Note\n",
           "that `extract_parameters()` can only work with slendr tree sequences.",
           call. = FALSE)
  } else if (inherits(data, "slendr_model"))
    model <- data

  result <- list()

  result[["splits"]] <- model$splits %>%
    dplyr::select(pop, parent, N, time = tsplit_orig, remove = tremove_orig) %>%
    dplyr::mutate(parent = ifelse(parent == "__pop_is_ancestor", NA, parent),
                  remove = ifelse(remove == -1, NA, remove))

  if (!is.null(model$geneflow))
    result[["gene_flows"]] <- model$geneflow %>%
      dplyr::select(from, to, start = tstart_orig, end = tend_orig, proportion)

  if (!is.null(model$resizes))
    result[["resizes"]] <- model$resizes %>%
      dplyr::select(pop, how, N, time = tresize_orig, end = tend_orig) %>%
      dplyr::mutate(end = ifelse(end == -1, NA, end))

  result
}
