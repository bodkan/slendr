.onAttach <- function(libname, pkgname) {
  # check for presence of the slim binary in user's PATH and display
  # a warning if it's not present
  path_check <- all(Sys.which("slim") != "")
  if (!path_check) {
    packageStartupMessage(
      "The slim binary was not found in your $PATH variable. Most of\n",
      "the functionality in this package will work without any issues\n",
      "but you will not be able to simulate data with the `slim()` function.\n",
      "\nMake sure to modify the $PATH variable in your .Renviron file so \n",
      "that it points to the directory containing the slim program.")
  }
}

# global references to required Python packages - inspired by:
# https://cran.r-project.org/web/packages/reticulate/vignettes/package.html
tskit <- NULL
pyslim <- NULL
msp <- NULL

.onLoad <- function(libname, pkgname) {
  tskit <<- reticulate::import("tskit", delay_load = TRUE)
  pyslim <<- reticulate::import("pyslim", delay_load = TRUE)
  msp <<- reticulate::import("msprime", delay_load = TRUE)
}
