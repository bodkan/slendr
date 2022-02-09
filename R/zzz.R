.onAttach <- function(libname, pkgname) {
  # check for presence of the slim binary in user's PATH and display
  # a warning if it's not present
  path_check <- all(Sys.which("slim") != "")
  if (!path_check) {
    packageStartupMessage(
      "The slim binary was not found in your $PATH variable. Most of\n",
      "the functionality in this package will work without any issues\n",
      "but you will not be able to simulate data with the `slim()` function.\n",
      "\nIf you set up a Python environment with `setup_env()`, you will be\n",
      "able to simulate non-spatial with slendr's msprime back end.\n",
      "\nIf you want to run SLiM spatial simulations, make sure to modify the $PATH\n",
      "variable in your ~/.Renviron file so that it points to the directory\n",
      "containing the slim command-line program.")
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
