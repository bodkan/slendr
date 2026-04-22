#' Activate the Python environment of slendr
#'
#' This function activates a dedicated slendr Python environment set up via
#' \code{setup_env}, or creates an ephemeral Python environment with uv.
#'
#' @param uv Should an ephemeral Python environment be created via uv (instead
#'   of activating a permanent virtual environment created via \code{setup_env})?
#' @param quiet Should informative messages be printed to the console? Default
#'   is \code{FALSE}.
#'
#' @return No return value, called for side effects
#'
#' @export
init_env <- function(uv = FALSE, quiet = FALSE) {
  if (uv || Sys.getenv("SLENDR_UV") == "TRUE") {
    reticulate::py_require(packages = DEPS$modules, python_version = deps$python)
  } else if (is_slendr_condaenv_present()) {
    reticulate::use_condaenv(DEPS$env)
  } else if (is_slendr_virtualenv_present()) {
    reticulate::use_virtualenv(DEPS$env)
  } else {
    stop("Could not activate a Python environment for slendr because it is\n",
         "not present on your system:\n\n", DEPS$env, "\n\n",
         "To set up a Python environment, you first need to run `setup_env()` or\n",
         "call `init_env(uv = TRUE)` to create an ephemeral Python environment.",
         call. = FALSE)
  }

  missing <- c(
    "msprime" = !reticulate::py_module_available("msprime"),
    "tskit" = !reticulate::py_module_available("tskit"),
    "pyslim" = !reticulate::py_module_available("pyslim"),
    "tspop" = !reticulate::py_module_available("tspop")
  )

  if (any(missing)) {
    which_missing <- paste(names(missing)[missing], collapse = ", ")
    tryCatch({
    packages <- reticulate::py_list_packages()[c("package", "version")]
    }, error = function(msg) {
      stop("uv value:", uv, "\n", "env value:", Sys.getenv("SLENDR_UV"), "\n",
       # cat(paste(capture.output(reticulate::py_config()), collapse = "\n")),
        call. = FALSE)
    })
    default_val <- getOption("warning.length")
    options(warning.length = 7000L)
    on.exit(options(warning.length = default_val))
    stop("A Python environment for slendr has been created but the following",
         " modules appear to be missing: ", which_missing,
         "\n\nPython environment summary:\n",
         paste(utils::capture.output(reticulate::py_config()), collapse = "\n"),
         "\n\nInstalled Python modules:\n",
         paste(utils::capture.output(print(packages)), collapse = "\n"),
         call. = FALSE)
  } else {
    # this is an awful workaround around the reticulate/Python bug which prevents
    # import_from_path (see zzz.R) from working properly -- I'm getting nonsensical
    #   Error in py_call_impl(callable, dots$args, dots$keywords) :
    #     TypeError: integer argument expected, got float
    # in places with no integer/float conversion in sight
    #
    # at least it prevents having to do things like:
    # reticulate::py_run_string("def get_pedigree_ids(ts): return [ind.metadata['pedigree_id']
    #                                                              for ind in ts.individuals()]")
    # (moved from ts_read() here because this is a better place for loading our Python functions)
    reticulate::source_python(file = system.file("pylib/pylib.py", package = "slendr"))

    # pylib <<- reticulate::import_from_path(
    #   "pylib",
    #   path = system.file("python", package = "slendr"),
    #   delay_load = TRUE
    # )

    if (!quiet)
      message("Python virtual environment for slendr has been activated.")
  }
}

#' Setup a dedicated Python virtual environment for slendr
#'
#' This function will setup a dedicated Python virtual environment for slendr,
#' either via a dedicated miniconda distribution or using a Python installation
#' already available on the system.
#'
#' @param env Should the Python virtual environment be created with conda (default)
#'   or using an already available Python interpreter and its built-in pip module?
#' @param quiet Should informative messages be printed to the console? Default
#'   is \code{FALSE}.
#' @param agree Automatically agree to all questions?
#' @param pip Should pip be used instead of conda for installing slendr's Python
#'   dependencies? (DEPRECATED)
#'
#' @return No return value, called for side effects
#'
#' @export
setup_env <- function(env = c("conda", "venv"), agree = FALSE, quiet = FALSE, pip = NULL) {
  if (!is.null(pip)) {
    warning("The `pip =` argument of `setup_env()` has been deprecated. If you\n",
            "want to create a slendr Python virtual environment without conda\n",
            "and using pip instead, please run `setup_env(env = \"venv\")`.", call. = FALSE)
  }

  env <- match.arg(env, choices = c("conda", "venv"))
  if (is_slendr_condaenv_present() || is_slendr_virtualenv_present()) {
    message("A Python virtual environment of slendr is already present. You can\n",
            "activate it by calling `init_env()`.")
  } else {
    if (agree)
      answer <- 2
    else
      answer <- utils::menu(
        c("No", "Yes"),
        title = paste0(
          "This function will create a completely isolated Python environment\n",
          "with the necessary Python dependencies for slendr.\n",
          "\nEverything will be installed into a completely separate location\n",
          "and your other Python installations won't be affected at all.\n",
          "Additionally, you can always wipe out the automatically created\n",
          "environment by running clear_env().\n\n",
          "Do you wish to proceed with the automated Python environment setup?")
        )
    if (answer == 2) {
      message("======================================================================")
      message("Installing a Python environment for slendr. Please wait until")
      message("the installation procedure finishes. Interrupting this process")
      message("could leave the installation in an inconsistent state.")
      message("======================================================================\n")
      Sys.sleep(10)

      if (env == "conda") {
        Sys.setenv(CONDA_PLUGINS_AUTO_ACCEPT_TOS = "yes")

        if (!dir.exists(reticulate::miniconda_path()))
          reticulate::install_miniconda()

        reticulate::conda_create(DEPS$env, python_version = DEPS$python)
        reticulate::use_condaenv(DEPS$env, required = TRUE)

        # tspop isn't available on conda and pyslim gives installation errors with conda
        # on M-architecture Macs, so they will need to be installed by pip
        # no matter the user's preference (given by the pip function argument value)
        # TODO: check at some point later if tspop / pyslim are on conda for all systems
        pip_only <- grepl("tspop|pyslim", DEPS$modules)
        reticulate::conda_install(envname = DEPS$env, packages = DEPS$modules[!pip_only], pip = FALSE)
        reticulate::conda_install(envname = DEPS$env, packages = DEPS$modules[pip_only], pip = TRUE)
      } else {
        reticulate::virtualenv_create(DEPS$env, packages = DEPS$modules)
        reticulate::use_virtualenv(DEPS$env, required = TRUE)
      }

      if (!quiet) {
        message("======================================================================")
        message("Python environment for slendr has been successfuly created and its\n",
                "interface to msprime, tskit, tspop, and pyslim has been activated.\n")
        message("In future sessions, activate this environment by calling init_env().")
        message("=======================================================================")
      }
    }
  }
}

#' Remove the automatically created slendr Python environment
#'
#' @param force Ask before deleting any environment?
#' @param all Should all (present and past) slendr Python environments be removed
#'   (default is \code{FALSE}) or just the current environment?
#'
#' @return No return value, called for side effects
#'
#' @export
clear_env <- function(force = FALSE, all = FALSE) {
  envs_to_delete <- reticulate::conda_list()$name %>%
    grep("Python-.*_msprime-.*_tskit-.*_pyslim-.*", ., value = TRUE)
  if (!all)
    envs_to_delete <- grep(DEPS$env, envs_to_delete, value = TRUE)

  if (length(envs_to_delete) == 0) {
    warning("No slendr Python environment has been found, nothing to delete.", call. = FALSE)
  } else {
    env_names <- paste(paste0("[#", seq_along(envs_to_delete), "]"), envs_to_delete)
    cat("The following slendr-looking Python environments have been identified:\n\n")
    cat(paste0(paste0(" - ", env_names, "\n"), collapse = ""))
    if (!force) {
      cat("\nYou will be asked for confirmation before any of them are deleted.\n")
      if (all == TRUE) {
        cat("To remove all of them at once, you can call `clear_env(force = TRUE, all = TRUE)`.\n")
      }
      cat("\n")
    }

    for (i in seq_along(envs_to_delete)) {
      env <- envs_to_delete[i]

      path <- reticulate::conda_list() %>%
        dplyr::filter(grepl(env, name)) %>%
        { gsub("bin\\/python", "", .$python) }

      if (force) {
        answer <- 2
      } else {
        answer <- utils::menu(
          c("No", "Yes"),
          title = paste0("Do you want to delete Python environment [#", i, "]? (Hit ESC to cancel.)\n\n", path)
        )
      }

      if (answer == 2) {
        reticulate::conda_remove(env)
        message("slendr environment '", env, "' has been sucessfully removed.")
      } else {
        message("slendr environment '", env, "' has not been removed.")
      }
    }
  }
}


#' Check that the active Python environment is setup for slendr
#'
#' This function inspects the Python environment which has been activated by the
#' reticulate package and prints the versions of all slendr Python dependencies
#' to the console.
#'
#' @param verbose Should a log message be printed? If \code{FALSE}, only a logical
#'   value is returned (invisibly).
#'
#' @return Either \code{TRUE} (slendr Python environment is present) or \code{FALSE}
#'   (slendr Python environment is not present).
#'
#' @examples
#' \dontshow{check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#' init_env()
#' check_env()
#' @export
check_env <- function(verbose = TRUE) {
  # if there is no Python available on user's system, don't immediately
  # jump to installing miniconda (let's deal with that in setup_env())
  # TODO: is this still needed now that reticulate prefers uv?
# orig_env <- Sys.getenv("RETICULATE_MINICONDA_ENABLED")
# Sys.setenv(RETICULATE_MINICONDA_ENABLED = FALSE)
# on.exit(Sys.setenv(RETICULATE_MINICONDA_ENABLED = orig_env))

  py <- reticulate::py_discover_config()

  has_tskit <- reticulate::py_module_available("tskit")
  has_msprime <- reticulate::py_module_available("msprime")
  has_pyslim <- reticulate::py_module_available("pyslim")
  has_tspop <- reticulate::py_module_available("tspop")
  # has_pylib <- !is.null(pylib)

  if (has_tskit)
    tskit_version <- paste("version", tskit[["_version"]]$tskit_version, "\u2713")
  else
    tskit_version <- "MISSING \u274C"

  if (has_msprime)
    msprime_version <- paste("version", msp[["_version"]]$version, "\u2713")
  else
    msprime_version <- "MISSING \u274C"

  if (has_pyslim)
    pyslim_version <- paste("version", pyslim$pyslim_version, "\u2713")
  else
    pyslim_version <- "MISSING \u274C"

  if (has_tspop)
    tspop_version <- paste("present \u2713")
  else
    tspop_version <- "MISSING \u274C"

  # if (has_pylib)
  #   pylib_status <- "successfully loaded \u2713"
  # else
  #   pylib_status <- "NOT LOADED \u274C"

  if (verbose) {
    cat("Summary of the currently active Python environment:\n\n")
    cat("Python binary:", py$python, "\n")
    cat("Python version:", py$version_string, "\n")

    cat("\nslendr requirements:\n")
    cat(" - tskit:", tskit_version, "\n")
    cat(" - msprime:", msprime_version, "\n")
    cat(" - pyslim:", pyslim_version, "\n")
    cat(" - tspop:", tspop_version, "\n")
    # cat(" - slendr module:", pylib_status, "\n")
  }

  if (!all(c(has_tskit, has_pyslim, has_msprime, has_tspop))) {
    return_value <- FALSE
    if (verbose)
      cat("\nNote that due to the technical limitations of embedded Python,",
          "if you\nwant to switch to another Python environment you will need",
          "to restart\nyour R session first.\n")
    # reference: https://github.com/rstudio/reticulate/issues/27#issuecomment-512256949
  } else
    return_value <- TRUE

  if (verbose)
    return(invisible(return_value))
  else
    return(return_value)
}

#' Check that the required dependencies are available for slendr to work
#'
#' @param python Is the slendr Python environment required?
#' @param slim Is SLiM required?
#' @param quit Should the R interpreter quit if required slendr dependencies are
#'   missing? This option (which is not turned on by default, being set to
#'   \code{FALSE}) is used mainly in avoiding running slendr man page examples on
#'   machines which lack dependencies. If set to \code{TRUE}, a logical value
#'   is returned.
#'
#' @return If \code{quit = TRUE}, no values is returned, if \code{quit = FALSE},
#'   a scalar logical value is returned indicating whether or not the dependencies
#'   are present.
#'
#' @export
check_dependencies <- function(python = FALSE, slim = FALSE, quit = FALSE) {
  # check whether SLiM and Python are present (only if needed!)
  missing_slim <- slim && !is_slim_present()
  missing_python <- (
    python &&
      Sys.getenv("SLENDR_UV") != "TRUE" &&
      !is_slendr_condaenv_present() &&
      !is_slendr_virtualenv_present()
    )

  fail <- missing_slim || missing_python

  if (fail) {
    if (quit)
      q()
    else
      return(FALSE)
  } else {
    return(invisible(TRUE))
  }
}

is_slendr_condaenv_present <- function() {
  tryCatch({ DEPS$env %in% reticulate::conda_list()$name }, error = function(cond) FALSE) }

is_slendr_virtualenv_present <- function() {
  tryCatch({ DEPS$env %in% reticulate::virtualenv_list() }, error = function(cond) FALSE)
}

#' Get a path to internal Python interpreter of slendr
#'
#' @return A character scalar path to slendr's Python binary
#'
#' @export
get_python <- function() {
  env_path <- file.path(reticulate::miniconda_path(), "envs", DEPS$env)
  if (Sys.info()["sysname"] == "Windows")
    python_path <- normalizePath(file.path(env_path, "python.exe"), winslash = "/", mustWork = FALSE)
  else
    python_path <- file.path(env_path, "bin", "python")
  python_path
}
