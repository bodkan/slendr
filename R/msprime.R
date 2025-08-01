#' Run a slendr model in msprime
#'
#' This function will execute a built-in msprime script and run a compiled
#' slendr demographic model.
#'
#' For more information about the \code{coalescent_only} argument, please see
#' msprime documentation, particularly the section on "Recording more information"
#' and the \code{coalescing_segments_only} argument of the method \code{sim_ancestry()}
#' here \url{https://tskit.dev/msprime/docs/stable/ancestry.html#recording-more-information}.
#' and \url{https://tskit.dev/msprime/docs/stable/api.html#msprime.sim_ancestry}.
#'
#' @param model Model object created by the \code{compile} function
#' @param sequence_length Total length of the simulated sequence (in base-pairs)
#' @param recombination_rate Recombination rate of the simulated sequence (in
#'   recombinations per basepair per generation)
#' @param samples A data frame of times at which a given number of individuals
#'   should be remembered in the tree-sequence (see \code{schedule_sampling} for a
#'   function that can generate the sampling schedule in the correct format). If
#'   missing, only individuals present at the end of the simulation will be
#'   recorded in the final tree-sequence file.
#' @param random_seed Random seed (if \code{NULL}, a seed will be generated between
#'   0 and the maximum integer number available)
#' @param verbose Write the log information from the SLiM run to the console
#'   (default \code{FALSE})?
#' @param debug Write msprime's debug log to the console (default \code{FALSE})?
#' @param run Should the msprime engine be run? If \code{FALSE}, the command line msprime
#'   command will be printed (and returned invisibly as a character vector) but not executed.
#' @param path Path to the directory where simulation result files will be saved.
#'   If \code{NULL}, this directory will be automatically created as a temporary
#'   directory. If \code{TRUE}, this path will be also returned by the function.
#'   If a string is given, it is assumed to be a path to a directory where simulation
#'   results will be saved. In this case, the function will return this path invisibly.
#'   Note that if a tree-sequence file should be simulated (along with other files,
#'   potentially), that tree-sequence file (named 'msprime.trees' by default) will
#'   have to be explicitly loaded using \code{ts_read()}.
#' @param coalescent_only Default is \code{TRUE}, which will only record the
#'   minimum amount of information necessary to represent the genealogical
#'   history of the simulated samples (i.e., only nodes which are MRCA of some
#'   pair of samples at some locus in the genome). Setting to \code{FALSE} will
#'   record much more information, resulting in unary nodes in the tree sequence.
#'   This parameter translates to the \code{coalescing_segments_only} argument
#'   of the underlying msprime method \code{sim_ancestry}. See Details for
#'   additional information.
#'
#' @return A tree-sequence object loaded via Python-R reticulate interface function \code{ts_read}
#'   (internally represented by the Python object \code{tskit.trees.TreeSequence}). If the
#'   \code{path} argument was set, it will return the path as a single-element character vector.
#'
#' @examples
#' \dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#' init_env()
#'
#' # load an example model
#' model <- read_model(path = system.file("extdata/models/introgression", package = "slendr"))
#'
#' # afr and eur objects would normally be created before slendr model compilation,
#' # but here we take them out of the model object already compiled for this
#' # example (in a standard slendr simulation pipeline, this wouldn't be necessary)
#' afr <- model$populations[["AFR"]]
#' eur <- model$populations[["EUR"]]
#' chimp <- model$populations[["CH"]]
#'
#' # schedule the sampling of a couple of ancient and present-day individuals
#' # given model at 20 ky, 10 ky, 5ky ago and at present-day (time 0)
#' modern_samples <- schedule_sampling(model, times = 0, list(afr, 10), list(eur, 100), list(chimp, 1))
#' ancient_samples <- schedule_sampling(model, times = c(40000, 30000, 20000, 10000), list(eur, 1))
#'
#' # sampling schedules are just data frames and can be merged easily
#' samples <- rbind(modern_samples, ancient_samples)
#'
#' # run a simulation using the msprime back end from a compiled slendr model object
#' ts <- msprime(model, sequence_length = 1e5, recombination_rate = 0, samples = samples)
#'
#' # simulated tree-sequence object can be saved to a file using ts_write()...
#' ts_file <- normalizePath(tempfile(fileext = ".trees"), winslash = "/", mustWork = FALSE)
#' ts_write(ts, ts_file)
#' # ... and, at a later point, loaded by ts_read()
#' ts <- ts_read(ts_file, model)
#'
#' summary(ts)
#' @export
msprime <- function(model, sequence_length, recombination_rate, samples = NULL,
                    random_seed = NULL, verbose = FALSE, debug = FALSE, run = TRUE,
                    path = NULL, coalescent_only = TRUE) {
  if (sequence_length %% 1 != 0 || sequence_length <= 0)
    stop("Sequence length must be a non-negative integer number", call. = FALSE)

  if (!is.numeric(recombination_rate) || recombination_rate < 0)
    stop("Recombination rate must be a non-negative numeric value", call. = FALSE)

  if (sum(model$splits$parent == "__pop_is_ancestor") > 1)
    stop("Multiple ancestral populations without a common ancestor would lead to\n",
         "an infinitely deep history without coalescence. Please make sure that all\n",
         "populations trace their ancestry to a single ancestral population.\n",
         "(This restriction only applies to coalescent simulations with msprime().)",
         call. = FALSE)

  if (!is.null(samples)) {
    samples <- process_sampling(samples, model, verbose)
    if (!is.null(model$path)) {
      sampling_path <- tempfile()
      readr::write_tsv(samples, sampling_path)
      sampling <- paste("--sampling-schedule", sampling_path)
    }
  } else
    sampling <- ""

  random_seed <- set_random_seed(random_seed)

  if (is.null(model$path)) {
    if (!run)
      stop("Impossible to run a non-serialized slendr model on the command line", call. = FALSE)

    script <- reticulate::import_from_path("script", path = system.file("scripts", package = "slendr"))
  } else {
    # verify checksums of serialized model configuration files
    checksums <- readr::read_tsv(file.path(model$path, "checksums.tsv"), progress = FALSE,
                                 col_types = "cc")
    verify_checksums(file.path(model$path, checksums$file), checksums$hash)

    script_path <- path.expand(file.path(model$path, "script.py"))
    if (!run) {
      msprime_command <- sprintf(
        "%s %s %s --model %s --sequence-length %d --recombination-rate %s %s %s %s %s --path %s",
        reticulate::py_exe(),
        script_path,
        paste("--seed", random_seed),
        model$path,
        sequence_length,
        recombination_rate,
        sampling,
        ifelse(verbose, "--verbose", ""),
        ifelse(debug, "--debug", ""),
        ifelse(coalescent_only, "--coalescent_only", ""),
        "path_to_a_trees_file.trees"
      )
      cat(msprime_command, "\n")
      return(invisible())
    } else {
      script <- reticulate::import_from_path("script", path = dirname(script_path))
    }
  }

  resizes <- if (is.null(model$resizes)) data.frame() else model$resizes
  geneflows <- if (is.null(model$geneflow)) data.frame() else model$geneflow
  if (is.null(samples)) samples <- data.frame()

  ts_msprime <- script$simulate(
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    seed = random_seed,
    populations = reticulate::r_to_py(model$splits),
    resizes = reticulate::r_to_py(resizes),
    geneflows = reticulate::r_to_py(geneflows),
    length = as.integer(model$length),
    orig_length = as.integer(model$orig_length),
    direction = model$direction,
    description = model$description,
    samples = reticulate::r_to_py(samples),
    debug = debug,
    coalescent_only = coalescent_only
  )
  ts_object <- ts_read(ts_msprime, model = model)

  if (is.null(path)) {
    return(ts_object)
  } else {
    if (!is.character(path))
      results_path <- file.path(tempdir(), paste0("slendr_results_", random_seed))
    else
      results_path <- path

    results_path <- normalizePath(results_path, winslash = "/", mustWork = FALSE)
    results_path <- paste0(results_path, "/")
    dir.create(results_path, recursive = TRUE, showWarnings = FALSE)

    ts_write(ts_object, normalizePath(file.path(results_path, "msprime.trees"), winslash = "/", mustWork = FALSE))

    if (is.logical(path) && path == TRUE)
      return(path)
    else
      return(invisible(path))
  }
}
