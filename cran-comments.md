# Resubmission #1

- All uses of `\dontrun{}` in examples have now been replaced by valid, fully runnable example code. A small simulated testing dataset has been included in the package to facilitate this.

- A default writing path is now never used in an argument to any function writing something to a disk. All writing (including in the examples, vignettes, and tests) is being done to a `tempdir()` or `tempfile()` by default.

- Every user-facing function's `.Rd` file now has a `\value` tag describing its output and the structure of such output. If a function is only run for side effects, this is now also formally stated.

- `on.exit()` is now used to restore `par()` options immediately on function exit.

- Calls to `installed.packages()` have been replaced by equivalent `requireNamespace()` as requested.

- The word 'Python' was put into single quotes in the DESCRIPTION.
- 'SLiM' is not a typo, this is how the software's name is supposed to be capitalized.

- Proper doi references have been added in DESCRIPTION to the software methods used in the package.

# R CMD check results

There were no ERRORs and no WARNINGs.

There was only one NOTE ("New submission").

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
