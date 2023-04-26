# # global references to required Python packages - inspired by:
# # https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
# # (Python environment initialization is now being done using init_env())
tskit <- NULL
pyslim <- NULL
msp <- NULL
# pylib <- NULL

# define slendr's required Python dependencies and compose an environment name
# that will be used specifically for them
PYTHON_ENV <-
  c("msprime==1.2.0", "tskit==0.5.4", "pyslim==1.0.1") %>%
  gsub("==", "-", .) %>%
  paste(collapse = "_") %>%
  paste0("Python-3.11_", .)

.onAttach <- function(libname, pkgname) {
  # check for presence of the slim binary in user's PATH and display
  # a warning if it's not present
  path_check <- all(Sys.which("slim") != "")
  if (!path_check) {
    packageStartupMessage(
      "The slim binary was not found in your $PATH variable. Most of\n",
      "the functionality in this package will work without any issues\n",
      "but you will not be able to simulate data with the `slim()` function.\n",
      "\nIf you want to run SLiM simulations, make sure to modify the $PATH\n",
      "variable so that it points to the directory containing the slim\n",
      "command-line program. One easy way to do this is to add this:\n\n",
      "PATH=\"path/to/the/directory/with/slim/program\"\n\n",
      "to your ~/.Renviron file.\n--------------------")
  } else {
    required_version <- "4.0"
    slim_version <- system("slim -v", intern = TRUE) %>%
      gsub("SLiM version (.*),.*$", "\\1", .) %>% .[1]
    if (utils::compareVersion(slim_version, required_version) < 0)
      packageStartupMessage(
        "You are running SLiM version ", slim_version,
        " but at least version ", required_version,
        "\nis required. Please upgrade SLiM to the latest version.\n--------------------"
      )
  }

  if (!is_slendr_env_present()) {
    if (!getOption("slendr.custom_env")) {
      version <- strsplit(PYTHON_ENV, "_")[[1]] %>% gsub(".*-", "", .)
      packageStartupMessage(
        sprintf(paste0("A slendr Python (%s) environment with the necessary versions of\n",
          "msprime (%s), tskit (%s), and pyslim (%s) has not been found.\n"),
          version[1], version[2], version[3], version[4]),
          "\nYou can setup a pre-configured environment with all of slendr's Python\n",
          "dependencies automatically by running the function setup_env()."
      )
    }
  }
}

.onLoad <- function(libname, pkgname) {
  # setup Python module imports (see also slendr's init_env() function)
  tskit <<- reticulate::import("tskit", delay_load = TRUE)
  pyslim <<- reticulate::import("pyslim", delay_load = TRUE)
  msp <<- reticulate::import("msprime", delay_load = TRUE)

  # setup slendr options (https://r-pkgs.org/r.html#when-you-do-need-side-effects)
  op <- options()
  op.slendr <- list(slendr.custom_env = FALSE)
  toset <- !(names(op.slendr) %in% names(op))
  if (any(toset)) options(op.slendr[toset])
  invisible()
}
