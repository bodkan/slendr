# global references to required Python packages - inspired by:
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
tskit <- NULL
pyslim <- NULL
msp <- NULL
# pylib <- NULL

# define slendr's required Python dependencies and compose an environment name
# that will be used specifically for them
deps <- c("msprime==1.1.1", "tskit==0.4.1", "pyslim==0.700")
PYTHON_ENV <- paste(gsub("==", "-", deps), collapse = "_")

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
    required_version <- "3.7.1"
    slim_version <- system("slim -v", intern = TRUE) %>%
      gsub("SLiM version (.*),.*$", "\\1", .)
    if (utils::compareVersion(slim_version, required_version) < 0)
      packageStartupMessage(
        "You are running SLiM version ", slim_version,
        " but at least version ", required_version,
        "\nis required. Please upgrade SLiM to the latest version.\n--------------------"
      )
  }

  if (!check_env_present()) {
    if (!getOption("slendr.custom_env")) {
      version <- gsub(".*==", "", deps)
      packageStartupMessage(
        sprintf(paste0("A slendr Python environment with the necessary versions of msprime (%s),\n",
        "tskit (%s), and pyslim (%s) has not been found. If this is not the\nfirst time you're ",
        "running slendr, the most likely explanation for this\nmessage is that some of slendr's ",
        "Python dependencies need an update.\n\n"),
        version[1], version[2], version[3]),
        "You can setup a pre-configured environment with all of slendr's Python\n",
        "dependencies by running the function setup_env()."
      )
    }
  } else
    setup_env()
}

.onLoad <- function(libname, pkgname) {
  tskit <<- reticulate::import("tskit", delay_load = TRUE)
  pyslim <<- reticulate::import("pyslim", delay_load = TRUE)
  msp <<- reticulate::import("msprime", delay_load = TRUE)
  # pylib <<- reticulate::import_from_path(
  #   "pylib",
  #   path = system.file("python", package = "slendr"),
  #   delay_load = TRUE
  # )

  # setup slendr options (https://r-pkgs.org/r.html#when-you-do-need-side-effects)
  op <- options()
  op.slendr <- list(slendr.custom_env = FALSE)
  toset <- !(names(op.slendr) %in% names(op))
  if (any(toset)) options(op.slendr[toset])
  invisible()
}
