# slendr (development version)

-   The format of the default *slendr* Python environment is now `msprime-<version>_tskit-<version>_pyslim-<version>_pandas`, dropping the `slendr_` prefix. This paves the way towards a future non-*slendr* tskit R package, which will share the same Python environment with *slendr* (because both R packages will go hand in hand). This isn't really a user-facing change, except that calling `setup_env()` will suggests creating a new Python environment and `library(slendr)` will appear as if a *slendr* environment is not yet present. Calling `setup_env()` and creating a new Python environment from scratch will solve the problem. ([#eb05180](https://github.com/bodkan/slendr/commit/eb05180))

-   `xrange` and `yrange` parameters of `world()` are now enforced to be two-dimensional numeric vectors, avoiding unnecessary issues with misspecified longitude/latitude ([#df95369](https://github.com/bodkan/slendr/commit/df95369))

-   The automated `setup_env()` function for creating dedicated mini Python environments for *slendr* now installs packages using *pip* by default. Reason: The rate of conda failures and dependency conflicts (even in the trivial case of installing nothing more than *msprime* + *tskit* + *pyslim* + *pandas*) is too high to rely on it. The option to use conda for package installations with `setup_env()` is still there, but the users must explicitly call `setup_env(pip = FALSE)` to get this behavior. Note that conda is still used as a means to install Python itself! This change only affects the way how Python modules are installed into a dedicated *slendr* Python environment, not the installation of Python itself. ([#81be1a7](https://github.com/bodkan/slendr/commit/81be1a7))

-   The name of the automatically created *slendr*-specific Python environment is now composed from the names *and versions* of Python modules installed. This makes it possible to naturally upgrade both *slendr* and its Python dependencies in case the *tskit* / *msprime* / *pyslim* folks ugprade some of those packages. In that case, if a *slendr* user upgrades the *slendr* package (and that new version requires newer versions of Python modules), *slendr* will simply recommend to create a new Python environment without additional effort on our part. ([#81be1a7](https://github.com/bodkan/slendr/commit/81be1a7))

-   The code of `setup_env()` was simplified to bare essentials. Now it *only* serves as a way to auto-setup a dedicated, isolated Python installation and *slendr* environment. The interface to install Python modules into custom-defined Python environment created outside R has been removed because this functionality is not necessary -- these custom environments can be easily activated by calling `reticulate::use_virtualenv` or `reticulate::use_condaenv`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

-   If some Python users want to use custom Python environments with *msprime*, *tskit*, and *pyslim*, they can silence the suggestion to use `setup_env()` printed by the `library(slendr)` call by setting `options(slendr.custom_env = TRUE)`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

# slendr 0.1.0

-   First numbered version of *slendr* to celebrate its [bioRxiv preprint](https://www.biorxiv.org/content/10.1101/2022.03.20.485041v1). 🥳 🎉
