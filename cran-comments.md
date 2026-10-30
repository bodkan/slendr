I made the unit testing suite more efficient to reduce the computational time of
CRAN checks. The issue of long run times was primarily caused by inefficiencies
in the process of Python environment activation by the 'reticulate' R package on
Windows.

# R CMD check results

There were no ERRORs, no WARNINGs, no NOTEs.

# Checked environments

* Local macOS install: R 4.6.1

* Win-Builder: R-devel, R-release, R-oldrelease

* GitHub Actions:
  * macOS: R-release
  * Ubuntu 20.04: R-devel, R-release, R-oldrelease
  * Ubuntu 18.04: R-oldrelease
  * Windows: R-release

# Downstream dependencies

None.
