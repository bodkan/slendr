## spammr: *S*patiotemporal *P*opulation *A*dmixture and *M*igration *M*odels in *R*

### ⚠️⚠️⚠️ WARNING ⚠️⚠️⚠️

**This software is still under heavy development!**

If you are interested in realistic spatio-temporal population genetic
simulations and somehow stumbled upon this repository then
congratulations! Your google-fu is strong.

The reason that this repository is even public is to share the results of
my work with collaborators without too much hassle. It is definitely not ready
for general usage.

If you would like to stay updated with the developments:

1. Click on the "Watch" button on the project's [Github
website](https://www.github.com/bodkan/spammr).

2. Follow me on [Twitter](https://www.twitter.com/fleventy5). I might
post some updates once the software is a bit more ready.

### Installation

You can install the package from Github using the `devtools` package:

```{r}
install.packages("devtools")
devtools::install_github("bodkan/spammr")
```

The package internally uses some of R's geospatial and geostatistical
libraries for geometric manipulation of population ranges, which
depend on external software. I have no way to test the installation on
a fresh system at the moment. It's possible the
`devtools::install_github()` procedure will crash on your machine. If
that happens, look for warnings of missing dependencies and install
them using your own system's package manager (for Mac users I
recommend [Homebrew](https://brew.sh/).

Also, the software depends on the latest version of the [SLiM simulation
framework](https://messerlab.org/slim/). To be specific, version 3.6 at the time
of writing. Please make sure to have this version installed - the R package will
not work otherwise.

