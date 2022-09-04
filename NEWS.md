# slendr (development version)

- The `msprime()` function now makes sure that a given _slendr_ model can fully coalesce to a single common ancestor population. Previously, having multiple ancestral populations created with `parent = "ancestor"` would cause an infinite simulation when plugged into the `msprime()` backend. ([#095b124](https://github.com/bodkan/slendr/commit/095b124))

- The initial size of a population which emerges from a split from another population is now printed in a population history summary in the R console. ([#6525bf3](https://github.com/bodkan/slendr/commit/6525bf3))

- A couple of fixes to support loading, processing, and plotting of "manually" created tree sequences have been implemented (see [this](https://tskit.dev/tutorials/tables_and_editing.html#constructing-a-tree-sequence)). Not sure how practically useful, but it's important to be able to load even "pure" tree sequences which are not from simulators such as SLiM and msprime. A set of [unit tests](https://github.com/bodkan/slendr/blob/9611437554bbb171f3df6374651acc3d73c63426/tests/testthat/test-manual-ts.R) has been added, making sure that a minimalist nodes & edges table can be loaded, as well as nodes & edges & individuals, plus tables of populations and sites & mutations. PRs with more extensive unit tests and bug reports of tree sequences which are failing to load would be appreciated! The code for handling cases of "manually-created" tree sequences which have missing individual table, missing populations table, etc. seems especially brittle at the moment ([#2f5fc32](https://github.com/bodkan/slendr/commit/2f5fc32)).

- The `-1` value as a missing value indicator used in tskit is now replaced with the more R-like `NA` in various tree-sequence tables (annotated by _slendr_ or original through tskit itself) ([#2f5fc32](https://github.com/bodkan/slendr/commit/2f5fc32)).

- Relative paths are now expanded in `ts_save()` ([#9521dfb](https://github.com/bodkan/slendr/commit/9521dfb)).

# slendr 0.3.0

-   SLiM 4.0 is now required for running simulations with the `slim()` engine. If you want to run _slendr_ simulations with SLiM (spatial or non-spatial), you will need to upgrade you SLiM installation. SLiM 3.7.1 version is no longer supported as the upcoming new _slendr_ spatial features will depend on SLiM 4.x and maintaining two functionally identical yet syntactically different back ends is not feasible (PR [#104](https://github.com/bodkan/slendr/pull/104)).

-   At the same time as the SLiM 4.0 release, new versions of Python modules msprime, tskit and pyslim have also been released. In fact, to be able to work with SLiM 4.0 tree sequences properly, those Python modules must be upgraded as well. Next time you load `library(slendr)`, you will be prompted to setup a new updated Python environment which you can do easily by running `setup_env()`.

-   Experimental support for running coalescent msprime simulations and analysing tree-sequence data using tskit on the Windows platform has now been implemented (PR [#102](https://github.com/bodkan/slendr/pull/102)).

# slendr 0.2.0

-   _slendr_ is now [on CRAN](https://CRAN.R-project.org/package=slendr)!

-   Big changes to the way tree-sequence outputs are handled by *slendr* by default. See [this comment](https://github.com/bodkan/slendr/pull/100#issue-1310869866) for an extended description and examples of the change. (PR [#100](https://github.com/bodkan/slendr/pull/100)). Briefly, simulation functions `slim()` and `msprime()` now return a tree-sequence object by default (can be switched off by setting `load = FALSE`), avoiding the need to always run `ts <- ts_load(model)` as previously. At the same time, a parameter `output = ` can be now used in `slim()` and `msprime()` to specify the location where a tree-sequence file should be saved (temporary file by default).

-   ***slendr*****'s tree-sequence R interface to the [tskit](https://tskit.dev/tskit/docs/stable/introduction.html) Python module has been generalized to load, process, and analyze tree sequences from non-*slendr* models!** This means that users can use the *slendr* R package even for analyzing tree sequences coming from standard msprime and SLiM scripts, including all spatial capabilities that have been only available for *slendr* tree sequences so far. Please note that this generalization is still rather experimental and there might be corner cases where a tree sequence from your msprime or SLiM script does not load properly or leads to other errors. If this happens, please open a GitHub issue with the script in question attached. (PR [#91](https://github.com/bodkan/slendr/pull/91))

-   Removed functions and some function arguments originally deprecated during the renaming phase of the pre-preprint refactoring. This affects `compile`, `boundary`, `dispersal`, `expand`, `geneflow`, `plot.slendr`, `plot_graph`, `read`, `sampling`, and `shrink`. Similarly, deprecated `dir` argument of the `compile_model` is now `path`, `geneflow` argument of `compile_model` is now `gene_flow`, and the `_dist` suffix was removed from `competition_dist`, `mate_dist`, and `dispersal_dist`. If you get an error about a missing function or a function argument in code which used to work in an ancient version of *slendr*, this is why. ([#985b451](https://github.com/bodkan/slendr/commit/985b451))

-   When setting up an isolated Python environment using `setup_env()`, *slendr* now makes a decision whether to install Python dependencies using pip (critical on osx-arm64 for which the conda msprime/tskit are unfortunately currently broken) or with conda (every other platform). This can be still influenced by the user using the `pip = <TRUE|FALSE>` argument, but we now change the default behavior on ARM64 Mac. ([#54a413d](https://github.com/bodkan/slendr/commit/54a413d))

-   The name of the default *slendr* Python environment is now shortened even more, and the redundant `_pandas` prefix is now dropped. **Users will be notified upon calling `library(slendr)` that a new environment should be created. This is OK, it's not a bug.** ([#54a413d](https://github.com/bodkan/slendr/commit/54a413d))

-   The format of the default *slendr* Python environment is now `msprime-<version>_tskit-<version>_pyslim-<version>_pandas`, dropping the `slendr_` prefix. This paves the way towards a future non-*slendr* tskit R package, which will share the same Python environment with *slendr* (because both R packages will go hand in hand). This isn't really a user-facing change, except that calling `setup_env()` will suggests creating a new Python environment and `library(slendr)` will appear as if a *slendr* environment is not yet present. Calling `setup_env()` and creating a new Python environment from scratch will solve the problem. ([#eb05180](https://github.com/bodkan/slendr/commit/eb05180))

-   `xrange` and `yrange` parameters of `world()` are now enforced to be two-dimensional numeric vectors, avoiding unnecessary issues with misspecified longitude/latitude ([#df95369](https://github.com/bodkan/slendr/commit/df95369))

- The argument `sampling = ` in `slim()` and `msprime()` is now renamed to `samples = ` ([#adf4e0d](https://github.com/bodkan/slendr/commit/adf4e0d)).

-   The automated `setup_env()` function for creating dedicated mini Python environments for *slendr* now installs packages using *pip* by default. Reason: The rate of conda failures and dependency conflicts (even in the trivial case of installing nothing more than *msprime* + *tskit* + *pyslim* + *pandas*) is too high to rely on it. The option to use conda for package installations with `setup_env()` is still there, but the users must explicitly call `setup_env(pip = FALSE)` to get this behavior. Note that conda is still used as a means to install Python itself! This change only affects the way how Python modules are installed into a dedicated *slendr* Python environment, not the installation of Python itself. ([#81be1a7](https://github.com/bodkan/slendr/commit/81be1a7))

-   The name of the automatically created *slendr*-specific Python environment is now composed from the names *and versions* of Python modules installed. This makes it possible to naturally upgrade both *slendr* and its Python dependencies in case the *tskit* / *msprime* / *pyslim* folks upgrade some of those packages. In that case, if a *slendr* user upgrades the *slendr* package (and that new version requires newer versions of Python modules), *slendr* will simply recommend to create a new Python environment without additional effort on our part. ([#81be1a7](https://github.com/bodkan/slendr/commit/81be1a7))

-   The code of `setup_env()` was simplified to bare essentials. Now it *only* serves as a way to auto-setup a dedicated, isolated Python installation and *slendr* environment. The interface to install Python modules into custom-defined Python environment created outside R has been removed because this functionality is not necessary -- these custom environments can be easily activated by calling `reticulate::use_virtualenv` or `reticulate::use_condaenv`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

-   If some Python users want to use custom Python environments with *msprime*, *tskit*, and *pyslim*, they can silence the suggestion to use `setup_env()` printed by the `library(slendr)` call by setting `options(slendr.custom_env = TRUE)`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

-   The argument `sim_length = ` is now renamed to `simulation_length = `. Both are accepted for the moment and using the old name will simply inform the user of the future deprecation. ([#56491fb](https://github.com/bodkan/slendr/commit/56491fb))

-   Extensive set of runnable examples including figures and a built-in pre-compiled example model have been added to the documentation. ([#395df62c](https://github.com/bodkan/slendr/commit/395df62c))

# slendr 0.1.0

-   First numbered version of *slendr* to celebrate its [bioRxiv preprint](https://www.biorxiv.org/content/10.1101/2022.03.20.485041v1). 🥳 🎉
