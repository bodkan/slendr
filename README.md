## spammr: *S*patiotemporal *P*opulation *A*dmixture and *M*igration *M*odels in *R*

### ⚠️⚠️⚠️ WARNING ⚠️⚠️⚠️

**This software is not even in an alpha stage yet.** The only reason
this repository is public is because I wanted to share it with my
collaborators without too much hassle.

If you are interested in realistic spatio-temporal population genetic
simulations and somehow stumbled upon this repository then
congratulations! Your google-fu is strong.

I suspect the software is about 60% towards being in an alpha
stage. It currently contains a draft implementation of visual
specification of spatial population maps (read: functions for creating
pretty and colorful geographic maps with population distributions). It
also has a barebones backend for using those spatial maps in
SLiM. Unfortunately, there is currently no or only very minimal
connection between those two parts. There is also no documentation
whatsoever and things tend to change several times during a week
pretty drastically.

If the above is not completely clear: **DO NOT USE THIS SOFTWARE JUST
YET**.

That said, if you would like to stay updated with the developments:

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