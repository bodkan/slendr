# slendr (development version)

- `ts_ibd()` now returns the ID number of a MRCA node of a pair of nodes sharing a given IBD segment, as well as the TMRCA of that node. ([#7e2825](https://github.com/bodkan/slendr/commit/7e2825))

- Trivial parameter errors are caught during `population()` calls rather than during simulation (solving minor issues discovered via big simulation runs during the development of [_demografr_](https://github.com/bodkan/demografr)). ([#e33373](https://github.com/bodkan/slendr/commit/e33373))

- Fix error in plotting exponential resizes which do not last until "the present". ([#4c49a4](https://github.com/bodkan/slendr/commit/4c49a4))

- `ts_ibd()` no longer gives obscure error when `between =` is provided as a named list of individuals' names (instead of an expected unnamed list). The names of list elements are not used in any way, but the error happens somewhere deeply in the R->Python translation layer inside [_reticulate_](https://rstudio.github.io/reticulate/) and there's no need for the users to concern themselves with it. ([#7965e4](https://github.com/bodkan/slendr/commit/7965e4))

- Population size parameters and times are now explicitly converted to integer numbers. This is more of an internal, formal change (the conversion has been happening implicitly inside the SLiM engine anyway) but is now explicitly stated, also in the documentation of each relevant function. ([#b7e89e](https://github.com/bodkan/slendr/commit/b7e89e))

- Population names are now restricted to only those strings which are also valid Python identifiers. Although this restriction is only needed for the msprime back end of _slendr_ (not SLiM), it makes sense to keep things tidy and unified. This fixes msprime crashing with `ValueError: A population name must be a valid Python identifier`. ([#4ef5184](https://github.com/bodkan/slendr/commit/4ef5184))

- The layout algorithm of `plot_model()` has been improved significantly. (PR [#135](https://github.com/bodkan/slendr/pull/135)).

# slendr 0.5.1

- This minor release implements an emergency fix for a CRAN warning which suddenly popped up in latest CRAN checks. ([#5600a4](https://github.com/bodkan/slendr/commit/5600a4))

- A new function `ts_ibd()` has been added, representing an R interface to the _tskit_ method `TreeSequence.ibd_segments()`. However, note that `ts_ibd()` returns IBD results as a data frame (optionally, a spatially annotated _sf_ data frame). The function does not operate around iteration, as does its Python counterpart in _tskit_. Until the next major version of _slendr_, this function should be considered experimental. (PR [#123](https://github.com/bodkan/slendr/pull/123))


# slendr 0.5.0

- **<u>Minor breaking change!</u> Python environments of _slendr_ are no longer automatically activated upon calling `library(slendr)`! Using the coalescent _msprime_ back end and _slendr_'s tree-sequence functions now requires making an explicit call to a new function `init_env()` after `library(slendr)` is executed.** (PR [#102](https://github.com/bodkan/slendr/pull/118))

Motivation for the change: A small proportion of users have been experiencing issues with broken conda environments and various other issues with Python virtual environments in general. It's hard to guess how frequent this has been, but experience from workshops and courses suggests perhaps 1 in 20 of users experiencing Python issues which hindered their ability to use _slendr_ .(Fun fact: the first user-submitted GitHub issue upon releasing the first version of the _slendr_ R package was... a Python virtual environment issue).

Explanation: Activating Python environments automatically upon calling `library(slendr)` has been a popular feature because it hid away most of the complexities of the R-Python interface that powers _slendr_'s tree-sequence functionality. This was particularly convenient for many _slendr_ users, particularly those who have no experience with Python at all.

Unfortunately, in cases where a Python virtual environments with tskit/msprime/pyslim on a user's system ended up corrupted (or if anything else at the Python level got broken), the automatic Python environment activation performed by the `library(slendr)` call failed and _slendr_ was not even loaded. Sadly, this completely pulled the rug from under _slendr_ and there was nothing that could be done about it from its perspective (the issue happened at a low-level layer of embedded-Python before _slendr_ could've been loaded into R). Solving these issues was not difficult for experienced users, but many _slendr_ users have no experience with Python at all, they have never used conda, they don't understand the concept of "Python virtual environments" or how the R-Python interface works. And nor should they! After all, _slendr_ is an R package.

Splitting the Python virtual environment activation step into its own `init_env()` function means that `library(slendr)` now always succeeds (regardless of potential underlying Python issues on a user's sytem), making it much easier to diagnose and fix Python problems from R once the package is loaded.

So, to recap: `library(slendr)` no longer activates _slendr_'s isolated Python virtual environment. In order to simulate tree sequences and analyse them using its interface to _tskit_, it is necessary to call `init_env()`. This function performs the same Python-activation steps that `library(slendr)` used to call automagically in earlier _slendr_ versions. No other change to your scripts is necessary.

- Related to the previous point: _slendr_ now requires Python 3.11, msprime 1.2.0, tskit 0.5.4, and pyslim 1.0.1, to keep up with recent releases of its Python dependencies. Again, this presents no hassle to the user, and the only thing required is re-running `setup_env()`. (PR [#112](https://github.com/bodkan/slendr/pull/121)).

- When a named list is provided as a `sample_sets =` argument to a oneway statistic function, the names are used in a `set` column of the resulting data frame even if only single samples were used. ([#2a6781](https://github.com/bodkan/slendr/commit/2a6781))

- It is now possible to have non-spatial populations in an otherwise spatial model. Of course, when plotting such models on a map, only spatial components of the model will be plotted and _slendr_ will give a warning. To be absolutely sure that users intends to do this, _slendr_ will also give a warning when running `compile_model()` on models like this. Please consider this option experimental for the time-being as it is hard to predict which edge cases might break because of this (all unit tests and documentation tests are passing though). Feedback is more than welcome. (PR [#112](https://github.com/bodkan/slendr/pull/121)).

- It is now possible to label groups of samples in _slendr_'s _tskit_ interface functions which should make data frames with statistics results more readable. As an example, running `ts_f3(ts, A = c("p1_1", "p1_2", "p1_3"), B = c("p2_1", "p2_3"), C = c("p3_1", "p3_2", "p3_"))` resulted in a following data-frame output:

```
> ts_f3(ts, A = c("p1_1", "p1_2", "p1_3", "p1_4", "p1_5"),
            B = c("p2_1", "p2_2", "p2_3"),
            C = c("p3_1", "p3_2", "p3_3", "p3_4"))

# A tibble: 1 × 4
  A                        B              C                         f3
  <chr>                    <chr>          <chr>                  <dbl>
1 p1_1+p1_2+p1_3+p1_4+p1_5 p2_1+p2_2+p2_3 p3_1+p3_2+p3_3+p3_4 0.000130
```

This gets unwieldy rather quickly, especially when dozens or hundreds of samples are grouped together as populations. The new syntax allows the following shortcut via customised group names leveraging the standard named `list` functionality in R:

```
> ts_f3(ts, A = list(group_one = c("p1_1", "p1_2", "p1_3", "p1_4", "p1_5")),
            B = list(group_two = c("p2_1", "p2_2", "p2_3")),
            C = list(group_three = c("p3_1", "p3_2", "p3_3", "p3_4")))
# A tibble: 1 × 4
  A         B         C                 f3
  <chr>     <chr>     <chr>          <dbl>
1 group_one group_two group_three 0.000130
```

This is more readable and in line with some other _tskit_-interface functions of _slendr_ which used this functionality via their `sample_sets = ` argument (`ts_divergence()`, `ts_diversity()`, etc.).  ([#ac5e484](https://github.com/bodkan/slendr/commit/ac5e484))

- The default state of the `parent = ` argument of `population()` is now `NULL` instead of `"ancestor"`. This prevents silly surprising clashes in situation where some population's name really _is_ "ancestor". The only change internally is that for populations which are ancestral, the `splits` data frame element of a _slendr_ model object which includes this population carries a formal "ancestral parent population" as `"__pop_is_ancestor"` instead of just `"ancestor"`. Note that this is an internal implementation detail and not something that particularly has to involve the user. Still, if you have been somehow using _slendr_'s internal data structures, keep this in mind. ([#f8a39a2](https://github.com/bodkan/slendr/commit/f8a39a2))

# slendr 0.4.0

- The `msprime()` function now makes sure that a given _slendr_ model can fully coalesce to a single common ancestor population. Previously, having multiple ancestral populations created with `parent = "ancestor"` would cause an infinite simulation when plugged into the `msprime()` backend. ([#095b124](https://github.com/bodkan/slendr/commit/095b124))

- The initial size of a population which emerges from a split from another population is now printed in a population history summary in the R console. ([#6525bf3](https://github.com/bodkan/slendr/commit/6525bf3))

- A couple of fixes to support loading, processing, and plotting of "manually" created tree sequences have been implemented (see [this](https://tskit.dev/tutorials/tables_and_editing.html#constructing-a-tree-sequence)). Not sure how practically useful, but it's important to be able to load even "pure" tree sequences which are not from simulators such as SLiM and msprime. A set of [unit tests](https://github.com/bodkan/slendr/blob/main/tests/testthat/test-manual-ts.R) has been added, making sure that a minimalist nodes & edges table can be loaded, as well as nodes & edges & individuals, plus tables of populations and sites & mutations. PRs with more extensive unit tests and bug reports of tree sequences which are failing to load would be appreciated! The code for handling cases of "manually-created" tree sequences which have missing individual table, missing populations table, etc. seems especially brittle at the moment ([#79adf14](https://github.com/bodkan/slendr/commit/79adf14)).

- The `-1` value as a missing value indicator used in tskit is now replaced with the more R-like `NA` in various tree-sequence tables (annotated by _slendr_ or original through tskit itself) ([#79adf14](https://github.com/bodkan/slendr/commit/79adf14)).

- Relative paths are now expanded in `ts_save()` ([#382e0b7](https://github.com/bodkan/slendr/commit/382e0b7)).

- _slendr_ models can now be optionally compiled without serialization to disk. This only works with the `msprime()` coalescent back end but will be much faster in cases where a huge number of simulations needs to be run because for non-serialized models, `msprime()` now calls the back end engine directly through the R-Python interface (rather than on the command line) and output tree sequences are not saved to disk, rather than passed through the Python-R interface directly in memory  (PR [#112](https://github.com/bodkan/slendr/pull/112)).

- Deprecated argument `sampling = ` of the functions `slim()` and `msprime()` has now been permanently removed in favour of the `samples =` argument ([#0757b6e](https://github.com/bodkan/slendr/commit/0757b6e)).

- Avoid the unnecessary `array` type of _tskit_ results returned via reticulate. Numeric vectors (columns of data frames with numerical results) obtained in this way are simple R numeric vector ([#5101b39](https://github.com/bodkan/slendr/commit/5101b39)).

- One-way and multi-way statistics results are now returned as simple numerical vectors. Previously, results were returned as a type `array` despite "looking" as vectors (this is how values are returned to R from the reticulate-Python layer), which caused unnecessary annoyances and type-conversions on the R side of things and was not even intended ([#403df3b](https://github.com/bodkan/slendr/commit/403df3b)).

- Computing population genetic statistics on named samples that are not present in a tree sequence (most likely typos) is now correctly caught and reported as an error ([#da7e0bb](https://github.com/bodkan/slendr/commit/da7e0bb)).

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
