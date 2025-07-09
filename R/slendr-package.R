#' A Simulation Framework for Spatiotemporal Population Genetics
#'
#' A framework for simulating spatially explicit genomic data which
#' leverages real cartographic information for programmatic and visual encoding
#' of spatiotemporal population dynamics on real geographic landscapes. Population
#' genetic models are then automatically executed by the 'SLiM' software behind
#' the scenes, using a custom built-in simulation 'SLiM' script. Additionally,
#' fully abstract spatial models not tied to a specific geographic location are
#' supported, and users can also simulate data from standard, non-spatial,
#' random-mating models. These can be simulated either with the 'SLiM' built-in
#' back-end script, or using an efficient coalescent population genetics simulator
#' 'msprime' with a custom-built 'Python' script bundled with the R package.
#' Simulated genomic data is saved in a tree-sequence format and can be loaded,
#' manipulated, and summarised using tree-sequence functionality via an R
#' interface to the 'Python' module 'tskit'. Complete model configuration,
#' simulation and analysis pipelines can be therefore constructed without
#' a need to leave the R environment, eliminating friction between disparate
#' tools for population genetic simulations and data analysis.
#'
#' You can find installation instructions, reference manual, and tutorials at
#' <https://slendr.net>.
#' @docType package
#' @name slendr
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
