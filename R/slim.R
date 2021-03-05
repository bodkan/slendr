#' Open the SLiM backend script in the SLiM gui
#'
#' @export
run_slimgui <- function() {
  # backend_script <- system.file("backend.slim", package = "spammr")
  backend_script <- "~/projects/spammr/inst/extdata/backend.slim"
  system(sprintf("open -a SLiMgui %s", backend_script))
}
