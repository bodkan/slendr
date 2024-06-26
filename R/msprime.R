#' Run a slendr model in msprime
#'
#' This function will execute a built-in msprime script and run a compiled
#' slendr demographic model.
#'
#' @param model Model object created by the \code{compile} function
#' @param sequence_length Total length of the simulated sequence (in base-pairs)
#' @param recombination_rate Recombination rate of the simulated sequence (in
#'   recombinations per basepair per generation)
#' @param samples A data frame of times at which a given number of individuals
#'   should be remembered in the tree-sequence (see \code{schedule_sampling} for a
#'   function that can generate the sampling schedule in the correct format). If
#'   missing, only individuals present at the end of the simulation will be
#'   recorded in the tree-sequence output file.
#' @param ts Path to the output tree-sequence file. If \code{NULL} (the default),
#'   tree sequence will be saved to a temporary file.
#' @param random_seed Random seed (if \code{NULL}, a seed will be generated between
#'   0 and the maximum integer number available)
#' @param load Should the final tree sequence be immediately loaded and returned?
#'   Default is \code{TRUE}. The alternative (\code{FALSE}) is useful when a tree-sequence
#'   file is written to a custom location to be loaded at a later point.
#' @param verbose Write the output log to the console (default \code{FALSE})?
#' @param debug Write msprime's debug log to the console (default \code{FALSE})?
#' @param run Should the msprime engine be run? If \code{FALSE}, the command line msprime
#'   command will be printed (and returned invisibly as a character vector) but not executed.
#'
#' @return A tree-sequence object loaded via Python-R reticulate interface function \code{ts_load}
#'   (internally represented by the Python object \code{tskit.trees.TreeSequence}). Optionally,
#'   depending on the value of the arguments \code{load =} or \code{run =}, nothing or a character
#'   vector, respectively.
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
#' # automatic loading of a simulated output can be prevented by `load = FALSE`, which can be
#' # useful when a custom path to a tree-sequence output is given for later downstream analyses
#' output_file <- normalizePath(tempfile(fileext = ".trees"), winslash = "/", mustWork = FALSE)
#' msprime(model, sequence_length = 1e5, recombination_rate = 0, samples = samples,
#'         ts = output_file, load = FALSE, random_seed = 42)
#' # ... at a later stage:
#' ts <- ts_load(output_file, model)
#'
#' summary(ts)
#' @export
msprime <- function(model, sequence_length, recombination_rate, samples = NULL,
                    ts = NULL, random_seed = NULL,
                    load = TRUE, verbose = FALSE, debug = FALSE, run = TRUE) {
  if (sequence_length %% 1 != 0 | sequence_length <= 0)
    stop("Sequence length must be a non-negative integer number", call. = FALSE)

  if (!is.numeric(recombination_rate) | recombination_rate < 0)
    stop("Recombination rate must be a numeric value", call. = FALSE)

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

  # call msprime back-end code directly for non-serialized models
  if (is.null(model$path)) {
    if (!run)
      stop("Impossible to run a non-serialized slendr model on the command line", call. = FALSE)

    script <- reticulate::import_from_path("script", path = system.file("scripts", package = "slendr"))

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
      debug = debug
    )
    ts_object <- ts_load(ts_msprime, model = model)

    if (!is.null(ts))
      ts_save(ts_object, normalizePath(ts, winslash = "/", mustWork = FALSE))

    return(ts_object)
  }

  if (is.null(ts) & !load)
    warning("No custom tree-sequence output path is given but loading a tree sequence from\n",
            "a temporary file after the simulation has been prevented", call. = FALSE)

  if (is.null(ts)) ts <- normalizePath(tempfile(fileext = ".trees"), winslash = "/", mustWork = FALSE)

  model_dir <- model$path
  if (!dir.exists(model_dir))
    stop(sprintf("Model directory '%s' does not exist", model_dir), call. = FALSE)


  # verify checksums of serialized model configuration files
  checksums <- readr::read_tsv(file.path(model_dir, "checksums.tsv"), progress = FALSE,
                               col_types = "cc")
  verify_checksums(file.path(model_dir, checksums$file), checksums$hash)

  script_path <- path.expand(file.path(model_dir, "script.py"))

  msprime_command <- sprintf(
    "%s %s %s --model %s --output %s --sequence-length %d --recombination-rate %s %s %s %s",
    reticulate::py_exe(),
    script_path,
    paste("--seed", random_seed),
    path.expand(model_dir),
    ts,
    sequence_length,
    recombination_rate,
    sampling,
    ifelse(verbose, "--verbose", ""),
    ifelse(debug, "--debug", "")
  )

  if (verbose || !run) {
    cat("--------------------------------------------------\n")
    cat("msprime command:\n\n")
    cat(msprime_command, "\n")
    cat("--------------------------------------------------\n\n")
  }

  if (!run) return(invisible(msprime_command))

  reticulate::py_run_string(sprintf("import os; os.system(r'%s')", msprime_command))

  # if (system(msprime_command, ignore.stdout = !verbose) != 0)
  #   stop("msprime simulation resulted in an error -- see the output above", call. = FALSE)

  if (!file.exists(ts))
    stop("Tree sequence was not found at the expected location:\n", ts, call. = FALSE)

  if (load) {
    if (verbose) {
      cat("Tree sequence was saved to:\n", ts, "\n")
      cat("Loading the tree-sequence file...\n")

    }

    ts <- ts_load(model, file = ts)
    return(ts)
  }
}
