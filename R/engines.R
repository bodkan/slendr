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
#' @param output Path to the output tree-sequence file. If \code{NULL} (the default),
#'   tree sequence will be saved to a temporary file.
#' @param random_seed Random seed (if missing, SLiM's own seed will be used)
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
#' \dontshow{check_dependencies(python = TRUE) # make sure dependencies are present
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
#' output_file <- tempfile(fileext = ".trees")
#' msprime(model, sequence_length = 1e5, recombination_rate = 0, samples = samples,
#'         output = output_file, load = FALSE, random_seed = 42)
#' # ... at a later stage:
#' ts <- ts_load(output_file, model)
#'
#' summary(ts)
#' @export
msprime <- function(model, sequence_length, recombination_rate, samples = NULL,
                    output = NULL, random_seed = NULL,
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
      direction = model$direction,
      description = model$description,
      samples = reticulate::r_to_py(samples),
      debug = debug
    )
    ts <- ts_load(ts_msprime, model = model)
    return(ts)
  }

  if (is.null(output) & !load)
    warning("No custom tree-sequence output path is given but loading a tree sequence from\n",
            "a temporary file after the simulation has been prevented", call. = FALSE)

  if (is.null(output)) output <- tempfile(fileext = ".trees")

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
    ifelse(is.null(random_seed), "", paste("--seed", random_seed)),
    path.expand(model_dir),
    output,
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

  if (!file.exists(output))
    stop("Tree sequence was not found at the expected location:\n", output, call. = FALSE)

  if (load) {
    if (verbose) {
      cat("Tree sequence was saved to:\n", output, "\n")
      cat("Loading the tree-sequence file...\n")

    }

    ts <- ts_load(model, file = output)
    return(ts)
  }
}


#' Run a slendr model in SLiM
#'
#' This function will execute a SLiM script generated by the \code{compile}
#' function during the compilation of a slendr demographic model.
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
#' @param output Path to the output tree-sequence file. If \code{NULL} (the default),
#'   tree sequence will be saved to a temporary file.
#' @param burnin Length of the burnin (in model's time units, i.e. years)
#' @param max_attempts How many attempts should be made to place an offspring
#'   near one of its parents? Serves to prevent infinite loops on the SLiM
#'   backend. Default value is 1.
#' @param spatial Should the model be executed in spatial mode? By default, if a
#'   world map was specified during model definition, simulation will proceed in
#'   a spatial mode.
#' @param coalescent_only Should \code{initializeTreeSeq(retainCoalescentOnly =
#'   <...>)} be set to \code{TRUE} (the default) or \code{FALSE}? See
#'   "retainCoalescentOnly" in the SLiM manual for more detail.
#' @param method How to run the script? ("gui" - open in SLiMgui, "batch" - run
#'   on the command line)
#' @param random_seed Random seed (if missing, SLiM's own seed will be used)
#' @param verbose Write the SLiM output log to the console (default
#'   \code{FALSE})?
#' @param load Should the final tree sequence be immediately loaded and returned?
#'   Default is \code{TRUE}. The alternative (\code{FALSE}) is useful when a tree-sequence
#'   file is written to a custom location to be loaded at a later point.
#' @param locations If \code{NULL}, locations are not saved. Otherwise, the
#'   path to the file where locations of each individual throughout the simulation
#'   will be saved (most likely for use with \code{animate_model}).
#' @param slim_path Optional way to specify path to an appropriate SLiM binary (this
#'   is useful if the \code{slim} binary is not on the \code{$PATH}).
#' @param run Should the SLiM engine be run? If \code{FALSE}, the command line SLiM
#'   command will be printed (and returned invisibly as a character vector) but not executed.
#'
#' @return A tree-sequence object loaded via Python-R reticulate interface function \code{ts_load}
#'   (internally represented by the Python object \code{tskit.trees.TreeSequence}). Optionally,
#'   depending on the value of the arguments \code{load =} or \code{run =}, nothing or a character
#'   vector, respectively.
#'
#' @examples
#' \dontshow{check_dependencies(python = TRUE, slim = TRUE) # make sure dependencies are present
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
#' modern_samples <- schedule_sampling(model, times = 0, list(afr, 5), list(eur, 5), list(chimp, 1))
#' ancient_samples <- schedule_sampling(model, times = c(30000, 20000, 10000), list(eur, 1))
#'
#' # sampling schedules are just data frames and can be merged easily
#' samples <- rbind(modern_samples, ancient_samples)
#'
#' # run a simulation using the SLiM back end from a compiled slendr model object and return
#' # a tree-sequence output
#' ts <- slim(model, sequence_length = 1e5, recombination_rate = 0, samples = samples)
#'
#' # automatic loading of a simulated output can be prevented by `load = FALSE`, which can be
#' # useful when a custom path to a tree-sequence output is given for later downstream analyses
#' output_file <- tempfile(fileext = ".trees")
#' slim(model, sequence_length = 1e5, recombination_rate = 0, samples = samples,
#'      output = output_file, load = FALSE)
#' # ... at a later stage:
#' ts <- ts_load(output_file, model)
#'
#' ts
#' @export
slim <- function(
  model, sequence_length, recombination_rate, samples = NULL, output = NULL,
  burnin = 0, max_attempts = 1, spatial = !is.null(model$world), coalescent_only = TRUE,
  method = c("batch", "gui"), random_seed = NULL,
  run = TRUE, verbose = FALSE, load = TRUE, locations = NULL, slim_path = NULL
) {
  method <- match.arg(method)

  if (is.null(model$path))
    stop("It is not possible to simulate non-serialized models in SLiM", call. = FALSE)

  if (is.null(output) & !load)
    warning("No custom tree-sequence output path is given but loading a tree sequence from\n",
            "a temporary file after the simulation has been prevented", call. = FALSE)

  if (is.null(output)) output <- tempfile(fileext = ".trees")

  if (method == "gui" & !interactive())
    stop("SLiMgui can only be run from an interactive R session", call. = FALSE)

  model_dir <- model$path
  if (!dir.exists(model_dir))
    stop(sprintf("Model directory '%s' does not exist", model_dir), call. = FALSE)

  if (sequence_length %% 1 != 0 | sequence_length <= 0)
    stop("Sequence length must be a non-negative integer number", call. = FALSE)

  if (!is.numeric(recombination_rate) | recombination_rate < 0)
    stop("Recombination rate must be a numeric value", call. = FALSE)

  # verify checksums of serialized model configuration files
  checksums <- readr::read_tsv(file.path(model_dir, "checksums.tsv"), progress = FALSE,
                               col_types = "cc")
  verify_checksums(file.path(model_dir, checksums$file), checksums$hash)

  if (is.character(slim_path) && !all(file.exists(slim_path)))
    stop("SLiM binary not found at ", slim_path, call. = FALSE)

  script_path <- path.expand(file.path(model_dir, "script.slim"))

  spatial <- if (spatial) "T" else "F"
  locations <- if (is.character(locations)) locations else ""
  coalescent_only <- if (coalescent_only) "T" else "F"
  burnin <- round(burnin / model$generation_time)

  sampling_path <- tempfile()
  sampling_df <- process_sampling(samples, model, verbose)
  readr::write_tsv(sampling_df, sampling_path)

  binary <- if (!is.null(slim_path)) slim_path else get_binary(method)
  if (binary != "open -a SLiMgui" && Sys.which(binary) == "")
    stop(sprintf("%s binary not found. Please modify your $PATH accordingly or
  specify the path manually by setting the 'binary_path' argument.", binary),
  call. = FALSE)

  seed <- if (is.null(random_seed)) "" else paste0(" \\\n    -d SEED=", random_seed)
  samples <- if (is.null(sampling_path)) ""
             else paste0(" \\\n    -d 'SAMPLES=\"", sampling_path, "\"'")

  if (method == "gui") {
    # to be able to execute the script in the SLiMgui, we have to hardcode
    # the path to the model configuration directory
    modif_path <- tempfile()
    readLines(script_path) %>%
      gsub("\"MODEL\", \".\"", paste0("\"MODEL\", \"", normalizePath(model$path), "\""), .) %>%
      gsub("\"SAMPLES\", \"\"", paste0("\"SAMPLES\", \"", normalizePath(sampling_path), "\""), .) %>%
      gsub("required_arg\\(\"OUTPUT_TS\"\\)", sprintf("defineConstant(\"OUTPUT_TS\", \"%s\")", output), .) %>%
      cat(file = modif_path, sep = "\n")
    system(sprintf("%s %s", binary, modif_path))
  } else {
    slim_command <- sprintf("%s %s %s \\
    -d 'MODEL=\"%s\"' \\
    -d 'OUTPUT_TS=\"%s\"' \\
    -d SPATIAL=%s \\
    -d SEQUENCE_LENGTH=%s \\
    -d RECOMB_RATE=%s \\
    -d BURNIN_LENGTH=%s \\
    -d SIMULATION_LENGTH=%s \\
    -d 'OUTPUT_LOCATIONS=\"%s\"' \\
    -d COALESCENT_ONLY=%s \\
    -d MAX_ATTEMPTS=%i \\
    %s 2>&1",
      binary, # path to the SLiM binary on the command line
      seed,
      samples,
      path.expand(model_dir),
      output,
      spatial,
      sequence_length,
      recombination_rate,
      burnin,
      model$length,
      locations,
      coalescent_only,
      max_attempts,
      script_path
    )

    if (verbose || !run) {
      cat("--------------------------------------------------\n")
      cat("SLiM command:\n\n")
      cat(slim_command, "\n")
      cat("--------------------------------------------------\n\n")
    }

    if (!run) return(invisible(slim_command))

    # execute the command, capture all log output and decide whether to print
    # any of the log information to the console
    log_output <- suppressWarnings(system(slim_command, intern = TRUE))
    log_warnings <- grep("WARNING", log_output, value = TRUE)
    if (verbose)
      cat(log_output, sep = "\n")
    else if (length(log_warnings)) {
      warning("There were some warnings during the simulation run:\n",
              paste(log_warnings, collapse = "\n"), call. = FALSE)
    }

    if (!grepl("simulation finished", log_output[length(log_output)])) {
      if (!verbose) cat(log_output, sep = "\n")
      stop("Unfortunately SLiM terminated before a tree sequence was saved.\n",
           "See the above for an indication of where things ",
           "could have gone wrong.",
           ifelse(!is.null(attr(log_output, "status")),
                  paste0("\n\nSLiM exit status: ",
                         attr(log_output, "status"), "\n",
                         "Message: ", attr(log_output, "errmsg")),
                  ""), call. = FALSE)
    }
  }

  # if the simulation was run in GUI mode, wait for the confirmation from the user that it
  # finished before loading the tree-sequence output file
  if (method == "gui")
    readline("Please confirm that the SLiMgui simulation is finished [press ENTER]")

  if (!file.exists(output))
    stop("Tree sequence was not found at the expected location:\n", output, call. = FALSE)

  if (load) {
    if (verbose) {
      cat("Tree sequence was saved to:\n", output, "\n")
      cat("Loading the tree-sequence file...\n")

    }

    ts <- ts_load(model, file = output)
    return(ts)
  }
}