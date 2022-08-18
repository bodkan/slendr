Two external dependencies of this R package (the SLiM simulation software and Python population genetic modules msprime/tskit/pyslim) have had new major releases soon after the first version of this package was accepted by CRAN.

My apologies for another submission this soon after submitting the very first version. This update makes sure that the R package is compatible with the updated external dependencies (all of which have unfortunately introduced backwards incompatible changes which could cause problems for some users).

Additionally, the formatting of the doi references in DESCRIPTION was changed according to the recommendation from a CRAN maintainer.

# R CMD check results

There were no ERRORs, no WARNINGs and no NOTEs.

# Checked environments

* Local macOS install: R 4.1.3

* Win-Builder: R-devel, R-release, R-oldrelease

* GitHub Actions:
  * macOS: R-release
  * Ubuntu 20.04: R-devel, R-release, R-oldrelease
  * Ubuntu 18.04: R-oldrelease
  * Windows: R-release

# Downstream dependencies

None.
