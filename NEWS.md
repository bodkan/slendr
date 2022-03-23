# slendr (development version)

* The code of `setup_env()` was simplified to bare essentials. Now it _only_ serves as a way to auto-setup a dedicated, isolated Python installation and _slendr_ environment. The interface to install Python modules into custom-defined Python environment created outside R has been removed because this functionality is not necessary -- these custom environments can be easily activated by calling `reticulate::use_virtualenv` or `reticulate::use_condaenv`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

* If some Python users want to use custom Python environments with _msprime_, _tskit_, and _pyslim_, they can silence the suggestion to use `setup_env()` printed by the `library(slendr)` call by setting `options(slendr.custom_env = TRUE)`. ([#30f24b9](https://github.com/bodkan/slendr/commit/30f24b9))

# slendr 0.1.0

* First numbered version of _slendr_ to celebrate its [bioRxiv preprint](https://www.biorxiv.org/content/10.1101/2022.03.20.485041v1). 🥳 🎉
