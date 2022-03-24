# slendr (development version)

* The automated `setup_env()` function for creating dedicated mini Python environments for _slendr_ now installs packages using _pip_ by default. Reason: The rate of conda failures and dependency conflicts (even in the trivial case of installing nothing more than _msprime_ + _tskit_ + _pyslim_ + _pandas_) is too high to rely on it. The option to use conda for package installations with `setup_env()` is still there, but the users must explicitly call `setup_env(pip = FALSE)` to get this behavior. Note that conda is still used as a means to install Python itself! This change only affects the way how Python modules are installed into a dedicated _slendr_ Python environment, not the installation of Python itself. ([#30f24b9](https://github.com/bodkan/slendr/commit/81be1a7))

* The name of the automatically created _slendr_-specific Python environment is now composed from the names _and versions_ of Python modules installed. This makes it possible to naturally upgrade both _slendr_ and its Python dependencies in case the _tskit_ / _msprime_ / _pyslim_ folks ugprade some of those packages. In that case, if a _slendr_ user upgrades the _slendr_ package (and that new version requires newer versions of Python modules), _slendr_ will simply recommend to create a new Python environment without additional effort on our part. ([#30f24b9](https://github.com/bodkan/slendr/commit/81be1a7))

* The code of `setup_env()` was simplified to bare essentials. Now it _only_ serves as a way to auto-setup a dedicated, isolated Python installation and _slendr_ environment. The interface to install Python modules into custom-defined Python environment created outside R has been removed because this functionality is not necessary -- these custom environments can be easily activated by calling `reticulate::use_virtualenv` or `reticulate::use_condaenv`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

* If some Python users want to use custom Python environments with _msprime_, _tskit_, and _pyslim_, they can silence the suggestion to use `setup_env()` printed by the `library(slendr)` call by setting `options(slendr.custom_env = TRUE)`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

# slendr 0.1.0

* First numbered version of _slendr_ to celebrate its [bioRxiv preprint](https://www.biorxiv.org/content/10.1101/2022.03.20.485041v1). 🥳 🎉
