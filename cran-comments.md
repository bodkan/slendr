I modified the unit testing suite so that less important tests are only ran on
my local test machines but are skipped on CRAN to reduce computational time.

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
