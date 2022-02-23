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
      "variable in your ~/.Renviron file so that it points to the directory\n",
      "containing the slim command-line program.\n--------------------")
  } else {
    required_version <- "3.7.1"
    slim_version <- system("slim -v", intern = TRUE) %>%
      gsub("SLiM version (.*),.*$", "\\1", .)
    if (utils::compareVersion(slim_version, required_version) < 0)
      packageStartupMessage(
        "You are running SLiM version ", slim_version,
        " but at least a version ", required_version,
        "\nis required. Please upgrade SLiM to the latest version.\n--------------------"
      )
  }
  if (!check_env_present()) {
    packageStartupMessage(
      "In order to setup a pre-configured Python environment with all\ndependencies",
      " for tree sequence analyses (Python modules tskit,\npyslim, and msprime)",
      " you can run the function setup_env().\n\nThis will install and configure a ",
      "completely isolated Python\nenvironment automatically for you, without affecting ",
      "your system\nat all."
    )
  }
}

# global references to required Python packages - inspired by:
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
tskit <- NULL
pyslim <- NULL
msp <- NULL
# pylib <- NULL

.onLoad <- function(libname, pkgname) {
  tskit <<- reticulate::import("tskit", delay_load = TRUE)
  pyslim <<- reticulate::import("pyslim", delay_load = TRUE)
  msp <<- reticulate::import("msprime", delay_load = TRUE)
  # pylib <<- reticulate::import_from_path(
  #   "pylib",
  #   path = system.file("python", package = "slendr"),
  #   delay_load = TRUE
  # )
}
