# slendr 1.2.0

- Due to new issues related to conda activating environments in an incorrect path which [started to pop up](https://github.com/bodkan/slendr/issues/179) (possibly due to a misfeature in the _reticulate_ package), activating procedure in _slendr_ was reverted to a slower, but apparently [more robust approach](https://github.com/bodkan/slendr/pull/182/commits/de868c160fb8eafd278676b98fd99edc90373c61). This will unfortunately make running massively parallelized simulations on Windows problematic due to a suspected-but-hard-to-detect race condition in conda on Windows, which manifested when executing many parallel environment activations (one for each simulation process) on this platform. For the time being, users are advised to execute big parallelized simulations (we're talking thousands of simulations in an ABC setting) on unix systems. ([PR #182](https://github.com/bodkan/slendr/pull/182))

- _slendr_ now allows recording of specific singular samples under unique names. See the documentation to `schedule_sampling()` for an example. ([PR #181](https://github.com/bodkan/slendr/pull/181))

- The dependency _rnaturalearth_ has introduced some low-level changes that required minor tweaks to _slendr_'s spatial internals. Thanks to @PMassicotte for his kind help on fixing this! ([PR #180](https://github.com/bodkan/slendr/pull/180))

- Fixed an issue with setting `lock = TRUE` in the `set_range()` function (issue #176 reported by @Sgornard -- thank you!). ([#6d01e407](https://github.com/bodkan/slendr/commit/6d01e407))

- If a non-spatial population is created which has a spatial parent population, _slendr_ gives an informative error message. ([#6bb646](https://github.com/bodkan/slendr/commit/6bb646))

- `plot_model()` now uses the same color for population ranges and population labels.  ([#c54632e5](https://github.com/bodkan/slendr/commit/c54632e5))

- `plot_map()` can now optionally visualize splits in a spatial setting ([#4664388c](https://github.com/bodkan/slendr/commit/4664388c))

- `plot_model()` now plots y-axis time using a more readable integer scale.  ([#6b0e7fcc](https://github.com/bodkan/slendr/commit/6b0e7fcc))

# slendr 1.1.0

A relatively modest release, mostly pushed out to keep _slendr_ in step with the recent release of SLiM 5.0 and new releases of  _msprime_ and _tskit_. As a result, _slendr_'s SLiM-based functionality (and particularly its new "SLiM extension" mechanism) requires SLiM 5.0

- SLiM 5 is now required to run `slim()` simulations with _slendr_. Similarly, _tskit_ and _msprime_ have been upgraded to versions 0.6.4 and 1.3.4, respectively. ([PR #173](https://github.com/bodkan/slendr/pull/173))

- `schedule_sampling()` now checks that all sampled populations are actually present in the compiled model ([#97abf75](https://github.com/bodkan/slendr/commit/97abf75))

- Data frames produced by `ts_tracts()` on _slendr_-produced tree sequences now include a time of a recorded sample carrying a given tract ([#a28b264](https://github.com/bodkan/slendr/commit/a28b264)) and a user-friendly haplotype number ([#9c4724](https://github.com/bodkan/slendr/commit/9c4724)) to which the tract belongs (i.e., either number 1 or 2, depending on the respective chromosome of a simulated individual). 

- `ts_vcf()` is now more robust (and more useful) in handling both _slendr_ and non-_slendr_ tree sequences. However, please take note of some potentially problematic situations involved in the _tskit_ VCF conversion method, and solutions discussed [here](https://github.com/bodkan/slendr/issues/167). Until this is all resolve, consider `ts_vcf()` in its current form rather experimental. Also, as a reminder, note that you can always use Python _tskit_ functionality functionality directly from R, using the _reticulate_ interface. ([#c26907](https://github.com/bodkan/slendr/commit/c26907))

- By default, `ts_vcf()` now automatically transforms a site with coordinate 0 to 1. This is done to make sure that the produced VCF file adheres to the VCF specification. See [relevant discussion](https://github.com/tskit-dev/tskit/issues/2838) in the _tskit_ repository for more information. ([#3a8c7a](https://github.com/bodkan/slendr/commit/3a8c7a))

- `get_python()` now replaces a `get_env()` function introduced in an earlier release. This function returns the full path to a Python binary which is part of the _slendr_ virtual Python environment. As such, it can be directly ran on the command line, and the resulting Python session will have all of the _slendr_ Python dependencies (_tskit_, _msprime_, etc.) available for import. ([#a76d3f](https://github.com/bodkan/slendr/commit/a76d3f))

- A new option `quiet =` now controls whether or not should `ts_genotypes()` write a message if any multiallelic sites are encountered during conversion from a tree-sequence object. ([#9260c6](https://github.com/bodkan/slendr/commit/9260c6))

# slendr 1.0.0

- **A massive update introducing the possibility of simulating non-neutral _slendr_ models with `slim()` has been introduced. This update is too big to describe in the changelog -- for more information and motivation, see the [description in the associated PR](https://github.com/bodkan/slendr/pull/155), or [the new extensive vignette](https://slendr.net/articles/vignette-11-extensions.html) on the topic. ([PR #155](https://github.com/bodkan/slendr/pull/155))**

**Implementing changes for the v1.0 release (particularly the support for non-neutral models) required changing _slendr_ internals at a very low level across the whole codebase. Feedback on this functionality, missing features, and bug reports are highly appreciated!**

Other changes:

- The behavior previously implemented via the `output =` and `ts =` arguments of `slim()` (and `msprime()`) has been changed to facilitate more straightforward handling of output paths in user-defined SLiM extensions and other packages leveraging _slendr_ for inference. The `slim()` and `msprime()` function interfaces are now simplified in the following way:

  - `slim()`: the `ts` argument is now logical. `TRUE` switches on tree-sequence recording, `FALSE` switches it off. If tree-sequence recording is on (the default setting), the function automatically returns a tree-sequence R object. If users want to save it to a custom location, they should use the function `ts_write()` on the returned tree-sequence object. If customized output files are to be produced via user-defined extension scripts, those scripts can use a _slendr_/SLiM constant `PATH`, which is always available in the built-in SLiM script and which can be set from R via `slim(..., path = <path to a directory>)`. In that case, the `slim()` function always returns that path back. Crucially, in this case `slim()` will not return a tree sequence object, but that object can be loaded by `ts_read("<path to a directory>/slim.trees")`. In other words, nothing changes for the usual SLiM-based _slendr_ workflow, but for models generating custom output files, a small amount of work is needed to load the tree sequence -- the tree-sequence file outputs are therefore treated exactly the same way as non-tree-sequence user-defined output files. As a result of these changes, `slim()` no longer accepts a `load = TRUE|FALSE` argument.

The above is implemented in PR [#157](https://github.com/bodkan/slendr/pull/157).

- `ts_genotypes()` now works even for non-_slendr_ tree sequences, which do not have _slendr_ individual names of samples in the `ts_nodes()` output. ([#d348ec](https://github.com/bodkan/slendr/commit/d348ec))

- Due to frequent issues with installation of Python dependencies of _slendr_ in a completely platform independent way (in the latest instance this being conda installation of pyslim crashing on M-architecture Macs), `setup_env()` now only uses conda to install _msprime_ and _tskit_ -- _pyslim_ and _tspop_ are always installed via pip regardless of whether `setup_env(pip = FALSE)` (the default) or `setup_env(pip = TRUE)` is used. ([#408948](https://github.com/bodkan/slendr/commit/408948))

- A new function `extract_parameters()` can extract parameters of either a compiled _slendr_ model object or a tree sequence simulated from a _slendr_ model. This can be useful particularly for simulation-based inferences where model parameters are often drawn from random distributions and there's a need to know which parameters of a model (split times, gene-flow rates, etc.) have been drawn. ([#3632bd0](https://github.com/bodkan/slendr/commit/3632bd0))

- `compile_model()` now allows to specify a description of time units used while scheduling _slendr_ model events. This has purely descriptive purpose -- in particular, these units are used in model plotting functions, etc. ([#9b5b7ea0](https://github.com/bodkan/slendr/commit/9b5b7ea0))

- The `slim_script` argument of `compile_model()` has been replaced by `extension` argument, which allows users to provide their custom-designed SLiM snippets for extending the behavior of _slendr_'s SLiM simulation engine. ([#d11ac7](https://github.com/bodkan/slendr/commit/d11ac7))

- The `sim_length` argument of `compile_model()` has been removed following a long period of deprecatiaon. ([#12da50](https://github.com/bodkan/slendr/commit/12da50))

- When a named list of samples is used as `X` input to `ts_f4ratio()`, the name of the element is used in the `X` column of the resulting data frame. ([#0571a6](https://github.com/bodkan/slendr/commit/0571a6))

- `ts_table()` can now extract the "sites" _tskit_ table as `ts_table(ts, "sites")`. ([#e708f2](https://github.com/bodkan/slendr/commit/e708f2))

- When applied to _slendr_ tree sequences, `ts_recapitate()` no longer issues the warning: `TimeUnitsMismatchWarning: The initial_state has time_units=ticks but time is measured in generations in msprime. This may lead to significant discrepancies between the timescales. If you wish to suppress this warning, you can use, e.g., warnings.simplefilter('ignore', msprime.TimeUnitsMismatchWarning)`. For _slendr_ tree sequences, ticks are the same thing as generations anyway. ([#43c45083](https://github.com/bodkan/slendr/commit/43c45083))

- Running `slim(..., method = "gui")` was broken due to recent changes to make _slendr_ work on Windows. A path to a generated SLiM script executed in SLiMgui was incorrectly normalized. Non-SLiMgui runs were not affected. ([#ccae1df](https://github.com/bodkan/slendr/commit/ccae1df))

# slendr 0.9.1

- A new helper function `get_env()` now returns the name of the built-in _slendr_ Python environment (without activating it). ([#162ccc](https://github.com/bodkan/slendr/commit/162ccc))

- `clear_env()` now has a new argument `all = (TRUE|FALSE)` which allows deleting all _slendr_ Python environments. Previously, the function always removed only the recent environment, which lead to the accumulation of potentially large number of _slendr_ environments over time. ([#8707b9](https://github.com/bodkan/slendr/commit/8707b9))

- `plot_model()` has a new optional argument `samples =` which will -- when set to a result of a sampling schedule created by `schedule_sampling()` -- visualize the counts of samples to be recorded at each given time-point. ([#d72ac5](https://github.com/bodkan/slendr/commit/d72ac5))

- The _msprime_ dependency of _slendr_ has been updated to version 1.3.1. As a result, `setup_env()` will have to be re-run to update the internal _slendr_ Python environment. ([#dcb83d](https://github.com/bodkan/slendr/commit/dcb83d))

# slendr 0.9.0

- A full support for running SLiM and _msprime_ simulations with _slendr_ and for analyzing tree sequences using its [_tskit_](https://tskit.dev) interface has been implemented. Please note that the Windows support is still rather experimental -- the internal _slendr_ test suite currently assumes that SLiM has been installed using the _msys2_ system as described in the section 2.3.1 of the SLiM manual and other means of installing SLiM (such as via conda) might require additional adjustments. A fallback option in the form of the `slim_path=` argument of the `slim()` function can be used in non-standard SLiM installation circumstances. For most convenience, please add the path to the directory containing the `slim.exe` binary to the `PATH` variable by editing the `C:/Users/<username>/Documents/.Renviron` file accordingly. See the relevant section on Windows installation in the [_slendr_ documentation](https://slendr.net/articles/vignette-00-installation.html) for additional information. Feedback on the Windows functionality and bug reports are highly appreciated via [GitHub](https://github.com/bodkan/slendr/issues) issues! **Many thanks to @GKresearch and @rdinnager for their huge help in making the Windows port happen!** (PR #149)

- A trivial change has been made to _slendr_'s SLiM back-end script fixing the issue introduced in a SLiM 4.1 upgrade (see changelog for version 0.8.1 below). This is not expected to lead to different simulation outputs between the two versions of slendr (0.8.2 vs 0.8.1) or SLiM (4.1 vs 4.0.1) used. (PR #148)

- The _msprime_ internal dependency of _slendr_ was updated to 1.3.0, and Python to 3.12. As a result, after loading _slendr_, users will be prompted to re-run `setup_env()` to make sure that the dedicated _slendr_ Python environment is fully updated. At the same time, this prevents a failing installation on (at the very least) M1 macOS using `pip`. ([#5ce212](https://github.com/bodkan/slendr/commit/5ce212), [#a210d4](https://github.com/bodkan/slendr/commit/a210d4))

# slendr 0.8.1

- Fixed an [issue](https://github.com/bodkan/slendr/issues/143) of apparent contradiction in time direction in models where range expansion was scheduled within some time interval together with associated "locked-in" changes in population size over that time interval. ([#d2a29e](https://github.com/bodkan/slendr/commit/d2a29e))

- The introduction of _tspop_ which is only installable via pip (see changelog for the previous version) caused GLIBCXX-related errors between conda and pip dependencies related to the pandas Python package. To work around this issue, `setup_env()` no longer installs _pandas_ from conda regardless of the setting of the `pip = TRUE|FALSE` parameter. Instead, _pandas_ is installed via pip in a single step when _tspop_ is being installed. ([#cbe960](https://github.com/bodkan/slendr/commit/cbe960))

---

**WARNING**: SLiM 4.1 which has just been released includes a couple of [backwards incompatible changes](https://github.com/MesserLab/SLiM/releases/tag/v4.1) related to the implementation of spatial maps which prevent the current version of _slendr_'s `slim()` function from working correctly. If you rely on the functionality provided by the `slim()` function, you will have to use SLiM 4.0. (Note that if you want to have multiple versions of SLiM on your system, you can either use the `slim_path =` argument of `slim()` or specify the `$PATH` to the required version of SLiM in your `~/.Renviron` file just like you do under normal circumstances). Porting _slendr_ for SLiM 4.1 is being worked on.

# slendr 0.8.0

- In order to support the new `ts_tracts()` function backed by the _tspop_ module (see the item below), a new _slendr_ Python environment is required. As such, users will have to run `setup_env()` to get all the required Python dependencies which will be now installed in the internal virtual environment named `Python-3.11_msprime-1.2.0_tskit-0.5.6_pyslim-1.0.4_tspop-0.0.2`. ([#b5330c](https://github.com/bodkan/slendr/commit/b5330c))

- Experimental support for the [_tspop_](https://tspop.readthedocs.io/en/latest/) `link-ancestors` algorithm for detection of ancestry tracts in the form of a new _slendr_ function `ts_tracts()`. Only works on _slendr_-generated _msprime_ tree sequences and "pure" _msprime_ and SLiM tree sequences (not _slendr_-generated SLiM tree sequences), and has been only tested on a few toy models. **Note:** the _tspop_ Python module is not published on conda. In order to set up a new _slendr_ Python environment, you will have to run `setup_env(pip = TRUE)` to make sure that Python dependencies are installed with pip instead of conda. (PR #145)

- Updated Python dependencies (bugfix pyslim release [v1.0.4](https://github.com/tskit-dev/pyslim/releases/tag/1.0.4) and tskit [v0.5.6](https://github.com/tskit-dev/tskit/releases/tag/0.5.6), the latter due to a broken `jsonschema` dependency of tskit). ([#001ee5](https://github.com/bodkan/slendr/commit/001ee5))

- Experimental support for [manually created](https://github.com/bodkan/slendr/pull/144) spatial tree sequences. (PR #144)

# slendr 0.7.2

- A new function `ts_names()` has been added, avoiding the need for the extremely frequent (and, unfortunately, cumbersome) trick of getting named lists of individual symbolic names `ts_samples(ts) %>% split(., .[[split]]) %>% lapply(`[[`, "name")` which is very confusing for all but the more experienced R users. ([#7db6ea](https://github.com/bodkan/slendr/commit/7db6ea))

- Fixed broken concatenation of symbolic sample names in tree-sequence statistic functions, when those were provided as unnamed single-element lists of character vectors. ([#b3c650](https://github.com/bodkan/slendr/commit/b3c650))

- `plot_model()` now has an argument `file =`, making it possible to save a visualization of a model without actually opening a plotting device. This can be useful particularly while working on a remote server, in order to avoid the often slow X11 rendering. ([#e60078](https://github.com/bodkan/slendr/commit/e60078))

- `plot_model()` now has an argument `order =` allowing to override the default [in-order](https://en.wikipedia.org/wiki/Tree_traversal#Inorder_traversal) ordering of populations along the x-axis. ([#7a10ea](https://github.com/bodkan/slendr/commit/7a10ea))

# slendr 0.7.1

- **Starting from this release, the \*spatial\* simulation and data analysis functionality of _slendr_ is conditional on the presence of R geospatial packages _sf_, _stars_, and _rnaturalearth_ on the system. This means that users will be able to install _slendr_ (and use all of its non-spatial functionality) even without having these R packages installed. That said, nothing really changes in practice: spatial features of _slendr_ are just one `install.packages(c("sf", "stars", "rnaturalearth"))` away! The difference is that _slendr_ doesn't try to do this during its own installation, but users are instructed to do this themselves (if needed) when the package is loaded.** ([#7a10ea](https://github.com/bodkan/slendr/commit/7a10ea))

If spatial dependencies are not present but a spatial _slendr_ function is called regardless (such as `world()`, `move()`, etc.), an error message is printed with an information on how to install spatial dependencies via `install.packages()` as above.

**Why?** It's true that the main reason for _slendr_'s existence is its ability to simulate spatio-temporal data on realistic landscapes via SLiM. However, in practice, most of the "average" uses of _slendr_ in the wild (and in classrooms!) rely on its traditional, non-spatial interface, with its spatial features being used comparatively rarely at the moment (except for some cutting-edge exploratory research). Given that setting up all of the spatial dependencies can be a bit of a hurdle, we have decided to make these dependencies optional, rather than force _every_ user to go through the process of their installation whether they need the spatial features or not.

- A function `check_dependencies()` is now exported and can be used to check whether a _slendr_ Python environment () or SLiM () are present. This is useful for other software building upon _slendr_, normal users can freely ignore this. ([#6ae6ce](https://github.com/bodkan/slendr/commit/6ae6ce))

- A path to a file from which a tree sequence was loaded from is now tracked internally via a `attr(<tree sequence>, "path")` attribute. Note that this has been implemented for the purposes of clean up for large-scale simulation studies (such as those facilitated by [_demografr_](https://github.com/bodkan/demografr/)) as a mostly internal feature, and should be considered experimental. ([#f181a2](https://github.com/bodkan/slendr/commit/f181a2))

- Attempts to resize a population right at the time of the split (which led to issues with simulations) are now prevented. ([#f181a2](https://github.com/bodkan/slendr/commit/f181a2))

- Fix for a minor issue preventing sampling an _msprime_ population right at the time of its creation. ([#aea231](https://github.com/bodkan/slendr/commit/aea231))



# slendr 0.7.0

**This is an emergency upgrade to match the latest pyslim 1.0.3 due to a serious bug in recapitation.** See [here](https://github.com/tskit-dev/pyslim/issues/307) and [here](https://github.com/bodkan/slendr/issues/141) for an extensive discussion during the process of identification of the bug and its eventual fix. For a brief summary of the practical consequences of this bug, see [this](https://ecoevo.social/@petrelharp/110583379684954818) thread by pyslim's developer and its formal announcement [here](https://groups.google.com/g/slim-discuss/c/Rtkkx_8pW58/m/PRyu9kpBAAAJ).

- This change will require you to re-run `setup_env()` in order to update slendr's Python internals by creating a new internal Python virtual environment. ([#45539a](https://github.com/bodkan/slendr/commit/45539a))

- A potential issue with a parent population being scheduled for removal before a daughter population splits from it is now caught at the moment of the daughter `population()` call rather than during a simulation `slim()` run. ([#0791b5](https://github.com/bodkan/slendr/commit/0791b5))

- The function `plot_model()` has a new argument `gene_flow=<TRUE|FALSE>` which determines whether gene-flow arrows will be visualized or not. ([#104aa6](https://github.com/bodkan/slendr/commit/104aa6))

- The possibility to perform recapitation, simplification, or mutation of a tree sequence right inside a call to `ts_read()` (by providing `recapitate = TRUE`, `simplify = TRUE`, and `mutate = TRUE`, together with their own arguments) has now been removed. The motivation for this change is the realization that there is no benefit of  doing things like `ts_read("<path>", recapitate = TRUE, Ne = ..., recombination_rate = ...)` over `ts_read("<path>") %>% ts_recapitate(Ne = ..., recombination_rate = ...)`, and the frequent confusion when `recapitate = TRUE` or other switches are forgotten by the user. All _slendr_ teaching material and most actively used research codebases I know of use the latter, more explicit, pipeline approach anyway, and this has been the one example where reduncancy does more harm than good. ([#ad82ee](https://github.com/bodkan/slendr/commit/ad82ee))

**Note:** Loading `library(slendr)` will prompt a message _"The legacy packages maptools, rgdal, and rgeos, underpinning the sp package, which was just loaded, will retire in October 2023. [...]."_ This is an internal business of packages used by _slendr_ which unfortunately cannot be silenced from _slendr_'s side. There's no reason to panic, you can safely ignore them. Apologies for the unnecessary noise.

# slendr 0.6.0

This is a relatively large update, which unfortunately had to be released in haste due to the [retirement of the _rgdal_ package](https://r-spatial.org/r/2023/05/15/evolution4.html) -- a significant dependency of the entire spatial R ecosystem which is being phased out in the effort to move towards modern low-level geospatial architecture. Although _slendr_ itself does not depend on _rgdal_, many of its dependencies used to (but won't in the short term, hence the push to remove the _rgdal_ dependency). The most significant update has been the addition of IBD functionality of _tskit_, as described below. However, large part of this functionality has not been extensively tested and should be considered extremely experimental at this stage. If you would like to use it, it might be safer to either wait for a later release in which the IBD functionality will be more stable, or use the underlying, battle-tested [Python implementation in _tskit_](https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.ibd_segments).

- `ts_ibd()` now returns the ID number of a MRCA node of a pair of nodes sharing a given IBD segment, as well as the TMRCA of that node. ([#7e2825](https://github.com/bodkan/slendr/commit/7e2825))

- Trivial parameter errors are caught during `population()` calls rather than during simulation (solving minor issues discovered via big simulation runs during the development of [_demografr_](https://github.com/bodkan/demografr)). ([#e33373](https://github.com/bodkan/slendr/commit/e33373))

- Fix error in plotting exponential resizes which do not last until "the present". ([#4c49a4](https://github.com/bodkan/slendr/commit/4c49a4))

- `ts_ibd()` no longer gives obscure error when `between =` is provided as a named list of individuals' names (instead of an expected unnamed list). The names of list elements are not used in any way, but the error happens somewhere deeply in the R->Python translation layer inside [_reticulate_](https://rstudio.github.io/reticulate/) and there's no need for the users to concern themselves with it. ([#7965e4](https://github.com/bodkan/slendr/commit/7965e4))

- Population size parameters and times are now explicitly converted to integer numbers. This is more of an internal, formal change (the conversion has been happening implicitly inside the SLiM engine anyway) but is now explicitly stated, also in the documentation of each relevant function. ([#b7e89e](https://github.com/bodkan/slendr/commit/b7e89e))

- Population names are now restricted to only those strings which are also valid Python identifiers. Although this restriction is only needed for the msprime back end of _slendr_ (not SLiM), it makes sense to keep things tidy and unified. This fixes msprime crashing with `ValueError: A population name must be a valid Python identifier`. ([#4ef518](https://github.com/bodkan/slendr/commit/4ef518))

- The layout algorithm of `plot_model()` has been improved significantly. (PR [#135](https://github.com/bodkan/slendr/pull/135)).

- A new optional argument `run =` has been added to `slim()` and `msprime()`. If set to `TRUE` (the default), the engines will operate the usual way. If set to `FALSE`, no simulation will be run and the functions will simply print a command-line command to execute the engine in question (returning the CLI command invisibly).  ([#2e5b85](https://github.com/bodkan/slendr/commit/2e5b85))

- The following start-up note is no longer shown upon calling `library(slendr)`:

```
    NOTE: Due to Python setup issues on some systems which have been
    causing trouble particularly for novice users, calling library(slendr)
    no longer activates slendr's Python environment automatically.
    In order to use slendr's msprime back end or its tree-sequence
    functionality, users must now activate slendr's Python environment
    manually by executing init_env() after calling library(slendr).
    (This note will be removed in the next major version of slendr.)
```

Users have to call `init_env()` to manually activate the Python environment of slendr (see note under version 0.5.0 below for an extended explanation).

- `ts_simplify()` now accepts optional arguments `keep_unary` and `keep_unary_in_individuals` (see the official [tskit docs](https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.simplify) for more detail) ([#1b2112](https://github.com/bodkan/slendr/commit/1b2112))

- Fix for `ts_read()` failing to load _slendr_-produced tree sequences after they were simplified down to a smaller set of sampled individuals (reported [here](https://github.com/bodkan/slendr/issues/136)). The issue was caused by incompatible sizes of the sampling table (always in the same form as used during simulation) and the table of individuals stored in the tree sequence after simplification (potentially containing a smaller set of individuals than in the original sampling table). To fix this, _slendr_ tree sequence objects now track information about which individuals are regarded as "samples" (i.e. those with symbolic names) which is maintained through simplification, serialization and loading, and used by _slendr_'s internal machinery during join operations. (PR [#137](https://github.com/bodkan/slendr/pull/137))

- Metadata summary of `ts_nodes()` results is no longer printed whenever typed into the R console. Instead, summary can be obtained by explicit call to `summary()` on the `ts_nodes()` tables.  ([#01af51](https://github.com/bodkan/slendr/commit/01af51)

- `ts_tree()` and `ts_phylo()` now extract trees based on tskit's own zero-based indexing [#554e13](https://github.com/bodkan/slendr/commit/554e13).

- `ts_simplify()` now accepts `filter_nodes = TRUE|FALSE`, with the same behavior to tskit's [own method](https://tskit.dev/tskit/docs/stable/python-api.html#tskit.TreeSequence.simplify) [#f07ffed](https://github.com/bodkan/slendr/commit/f07ffed).

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

- Relative paths are now expanded in `ts_write()` ([#382e0b7](https://github.com/bodkan/slendr/commit/382e0b7)).

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

-   Big changes to the way tree-sequence outputs are handled by _slendr_ by default. See [this comment](https://github.com/bodkan/slendr/pull/100#issue-1310869866) for an extended description and examples of the change. (PR [#100](https://github.com/bodkan/slendr/pull/100)). Briefly, simulation functions `slim()` and `msprime()` now return a tree-sequence object by default (can be switched off by setting `load = FALSE`), avoiding the need to always run `ts <- ts_read(model)` as previously. At the same time, a parameter `output = ` can be now used in `slim()` and `msprime()` to specify the location where a tree-sequence file should be saved (temporary file by default).

-   _slendr_'s tree-sequence R interface to the [tskit](https://tskit.dev/tskit/docs/stable/introduction.html) Python module has been generalized to load, process, and analyze tree sequences from non-_slendr_ models!** This means that users can use the _slendr_ R package even for analyzing tree sequences coming from standard msprime and SLiM scripts, including all spatial capabilities that have been only available for _slendr_ tree sequences so far. Please note that this generalization is still rather experimental and there might be corner cases where a tree sequence from your msprime or SLiM script does not load properly or leads to other errors. If this happens, please open a GitHub issue with the script in question attached. (PR [#91](https://github.com/bodkan/slendr/pull/91))

-   Removed functions and some function arguments originally deprecated during the renaming phase of the pre-preprint refactoring. This affects `compile`, `boundary`, `dispersal`, `expand`, `geneflow`, `plot.slendr`, `plot_graph`, `read`, `sampling`, and `shrink`. Similarly, deprecated `dir` argument of the `compile_model` is now `path`, `geneflow` argument of `compile_model` is now `gene_flow`, and the `_dist` suffix was removed from `competition_dist`, `mate_dist`, and `dispersal_dist`. If you get an error about a missing function or a function argument in code which used to work in an ancient version of _slendr_, this is why. ([#985b451](https://github.com/bodkan/slendr/commit/985b451))

-   When setting up an isolated Python environment using `setup_env()`, _slendr_ now makes a decision whether to install Python dependencies using pip (critical on osx-arm64 for which the conda msprime/tskit are unfortunately currently broken) or with conda (every other platform). This can be still influenced by the user using the `pip = <TRUE|FALSE>` argument, but we now change the default behavior on ARM64 Mac. ([#54a413d](https://github.com/bodkan/slendr/commit/54a413d))

-   The name of the default _slendr_ Python environment is now shortened even more, and the redundant `_pandas` prefix is now dropped. **Users will be notified upon calling `library(slendr)` that a new environment should be created. This is OK, it's not a bug.** ([#54a413d](https://github.com/bodkan/slendr/commit/54a413d))

-   The format of the default _slendr_ Python environment is now `msprime-<version>_tskit-<version>_pyslim-<version>_pandas`, dropping the `slendr_` prefix. This paves the way towards a future non-_slendr_ tskit R package, which will share the same Python environment with _slendr_ (because both R packages will go hand in hand). This isn't really a user-facing change, except that calling `setup_env()` will suggests creating a new Python environment and `library(slendr)` will appear as if a _slendr_ environment is not yet present. Calling `setup_env()` and creating a new Python environment from scratch will solve the problem. ([#eb05180](https://github.com/bodkan/slendr/commit/eb05180))

-   `xrange` and `yrange` parameters of `world()` are now enforced to be two-dimensional numeric vectors, avoiding unnecessary issues with misspecified longitude/latitude ([#df95369](https://github.com/bodkan/slendr/commit/df95369))

- The argument `sampling = ` in `slim()` and `msprime()` is now renamed to `samples = ` ([#adf4e0d](https://github.com/bodkan/slendr/commit/adf4e0d)).

-   The automated `setup_env()` function for creating dedicated mini Python environments for _slendr_ now installs packages using *pip* by default. Reason: The rate of conda failures and dependency conflicts (even in the trivial case of installing nothing more than *msprime* + *tskit* + *pyslim* + *pandas*) is too high to rely on it. The option to use conda for package installations with `setup_env()` is still there, but the users must explicitly call `setup_env(pip = FALSE)` to get this behavior. Note that conda is still used as a means to install Python itself! This change only affects the way how Python modules are installed into a dedicated _slendr_ Python environment, not the installation of Python itself. ([#81be1a7](https://github.com/bodkan/slendr/commit/81be1a7))

-   The name of the automatically created _slendr_-specific Python environment is now composed from the names *and versions* of Python modules installed. This makes it possible to naturally upgrade both _slendr_ and its Python dependencies in case the *tskit* / *msprime* / *pyslim* folks upgrade some of those packages. In that case, if a _slendr_ user upgrades the _slendr_ package (and that new version requires newer versions of Python modules), _slendr_ will simply recommend to create a new Python environment without additional effort on our part. ([#81be1a7](https://github.com/bodkan/slendr/commit/81be1a7))

-   The code of `setup_env()` was simplified to bare essentials. Now it *only* serves as a way to auto-setup a dedicated, isolated Python installation and _slendr_ environment. The interface to install Python modules into custom-defined Python environment created outside R has been removed because this functionality is not necessary -- these custom environments can be easily activated by calling `reticulate::use_virtualenv` or `reticulate::use_condaenv`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

-   If some Python users want to use custom Python environments with *msprime*, *tskit*, and *pyslim*, they can silence the suggestion to use `setup_env()` printed by the `library(slendr)` call by setting `options(slendr.custom_env = TRUE)`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

-   The argument `sim_length = ` is now renamed to `simulation_length = `. Both are accepted for the moment and using the old name will simply inform the user of the future deprecation. ([#56491fb](https://github.com/bodkan/slendr/commit/56491fb))

-   Extensive set of runnable examples including figures and a built-in pre-compiled example model have been added to the documentation. ([#395df62c](https://github.com/bodkan/slendr/commit/395df62c))

# slendr 0.1.0

-   First numbered version of _slendr_ to celebrate its [bioRxiv preprint](https://www.biorxiv.org/content/10.1101/2022.03.20.485041v1). 🥳 🎉
