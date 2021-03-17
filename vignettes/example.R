#' ---
#' title: "A full example of a spatial population model"
#' author: "Martin Petr"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'  %\VignetteEncoding{UTF-8}
#'  %\VignetteIndexEntry{A full example of a spatial population model}
#'  %\VignetteEngine{knitr::rmarkdown}
#' ---

#+ include = F
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi = 100
)

#+ include = F
devtools::load_all(".") # general readers: ignore this (mp)

#' # Introduction
#'
#' This vignette presents a much more complete example of a spatio-temporal
#' population model with admixture. Unlike the [main tutorial](tutorial.html),
#' it doesn't go into too much detail to explain the basic concepts. If you're
#' struggling with some of the features presented here, please take a look at
#' the [tutorial](tutorial.html) first.
#'
#' Note that this example script is supposed to demonstrate all the features of
#' `spammr` but its aim is not to be particularly realistic. Some migration
#' aspects are overly dramatic, but that's just for demonstration purposes.
#'
#' The raw script behind this vignette can be found
#' [here](https://raw.githubusercontent.com/bodkan/spammr/main/vignettes/example.R).
#' You might want to load this in RStudio to run the commands manually, instead
#' of copy-pasting individual pieces of the code from this website to the R
#' console yourself. The Markdown comments are still present in that script.
#'
#' ## Requirements
#'
#' **This R package requires SLiM version 3.6** and will not work with an older
#' release. You can find the latest version [here](https://messerlab.org/slim/).
#'
#' ️⚠️**Also, please make sure to always install the latest version from Github
#' before trying out the package by running:**

#+ eval = F
devtools::install_github("bodkan/spammr")

#' I'm adding bugfixes and new features every couple of hours and it's always
#' worth updating to the latest version.
#'
#' After you have installed to the latest version, you can load the
#' package by running:

#+ eval = F
library(spammr)

#' ## Define the world context

world <- map(
  xrange = c(-15, 60), # min-max longitude
  yrange = c(20, 65),  # min-max latitude
  crs = "EPSG:3035"    # real projected CRS used internally
)
plot(world)

#' ## Define some useful geographic regions
#'
#' As we will se below and in the [tutorial](tutorial.html), there are several
#' options to define population spatial ranges using geometric objects (circles,
#' arbitrary polygons, etc.). Defining a population range using a geographic
#' region object such as those we create here is another useful option.
#'
#' Note that these are _not_ population boundaries (not yet anyway)! These mean
#' nothing else but labeling some generic geographic boundary which can be used
#' later. They are not attached to any population at this point.
#'
#' ### Part of the African continent in our zoom of the world

africa <- region(
  "Africa", world,
  coords = list(
    c(-18, 20), c(40, 20), c(30, 33),
    c(20, 32), c(10, 35), c(-8, 35)
  )
)
plot(africa)

#' ### Europe and Anatolian peninsula:

europe <- region(
  "Western Europe", world,
  coords = list(c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
                c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)
plot(europe)

anatolia <- region(
  "Anatolia", world,
  coords = list(c(28, 35), c(40, 35), c(42, 40),
                c(30, 43), c(27, 40), c(25, 38))
)
plot(anatolia)

#' ### Europe _and_ Anatolia
#'
#' We use this region to restrict Anatolian "farmer" migration to Europe further
#' down in this vignette:

europe_anatolia <-region(
  "Western Europe & Anatolia", world,
  coords = list(c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
                c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50))
)
plot(europe_anatolia)

#' ## Define population dynamics
#'
#' Again, we can't go into too much detail here but what we demonstrate is:
#'
#' - definition of population ranges - `population()`
#' - migration of population through space - `move()`
#' - expansion of a population from its current location outwards - `expand()`
#' - changing a population spatial range at a certain time point - `update()`
#'
#' Look for the use of these functions for defining different spatio-temporal
#' population boundaries below.
#'
#' Note that the $N_e$ values are very small - this is on purpose, to make the
#' simulations run faster for demonstration purposes.
#'
#' ### African population of modern human ancestors:

afr <- population(
  "AFR", parent = "ancestor", Ne = 1000,
  world = world, region = africa
)

#' Below we can see how this object is represented when printed out. Under the
#' hood it is much more complicated, but this serves as a easy-to-read summary
#' for spotting obvious mistakes.

afr

#' Obviously, we can also plot the population range in the context of the
#' defined world:

plot(afr)

#' ### Out-of-Africa migrants
#'
#' This population starts in north east Africa and moves in a deliberately
#' funny trajectory somewhere to central Eurasia (perhaps the Ust-Ishim
#' population?). Note that the trajectory is specified using "checkpoints" -
#' points along the way between which the population moves.

ooa <- population(
  "OOA", parent = afr, time = 51000, Ne = 200,
  center = c(30, 30), radius = 300, remove = 27000
) %>% move(
  trajectory = list(c(40, 30), c(50, 30), c(60, 40), c(70, 40)),
  start = 50000,
  end = 40000,
  snapshots = 30
)

ooa

plot(ooa)

#' ### Eastern hunter-gatherers

ehg <- population(
  "EHG", time = 28000, Ne = 400, parent = ooa,
  world,
  coords = list(
    c(26, 55), c(38, 53), c(48, 53), c(60, 53),
    c(60, 60), c(48, 63), c(38, 63), c(26, 60)
  ),
  remove = 6000
)

ehg

plot(ehg)

#' ### Western hunter-gatherers/Neolithic farmers/present-day Europeans
#'
#' Those three populations were obviously very different from one another, but
#' they broadly occupied the same geographic range. We're clumping them here in
#' a single spatial boundary called "EUR", but they will obviously change over
#' time through admixture with other populations.

eur <- population(
  name = "EUR", time = 25000, Ne = 300, parent = ehg,
  world, region = europe
)

eur

plot(eur)

#' ### Caucasian hunter-gatherers/Anatolian farmers
#'
#' Again, we're oversimplifying here on purpose.

ana <- population(
  name = "ANA", time = 28000, Ne = 800, parent = ooa,
  world, center = c(34, 38), radius = 700,
  region = anatolia, remove = 6000
) %>% expand(
  by = 2500,
  start = 10000,
  end = 7000,
  snapshots = 10,
  region = europe_anatolia
)

ana

plot(ana)

#' ### Yamnaya steppe people

yam <- population(
  name = "YAM", time = 7000, Ne = 600, parent = ehg,
  world, coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  ),
  remove = 2000
)

yam

plot(yam)

#' ### Yamnaya invading Europe
#'
#' To model the migration of the Yamnaya into Europe, we're splitting a new
#' "YAM_migr" population from the original Yamnaya. The latter will stay where
#' it is, while the migrating population will start invading the spatial range
#' of Europeans ("Neolithic" population at the time):

yam_migr <- population(
  name = "YAM_migr", time = 6000, Ne = 1000, parent = yam,
  world, coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  ),
  remove = 2900
) %>%
  move(
    trajectory = c(15, 50),
    start = 5000,
    end = 3000,
    snapshots = 8
  )

yam_migr

plot(yam_migr)

#' ## Complete summary of spatial boundaries

#' The overloaded `plot()` (in fact, `plot.spammr()`) function is reasonably
#' smart in terms of plotting different geographic or population ranges at once.
#' We can use this to summarize the entire model at once. Obviously, this is a
#' lot of multi-dimensional information so the plot is definitely not perfect,
#' but it should provide a reasonably compact overview of what we've built so
#' far.

#+ fig.width = 7, fig.height = 9
plot(afr, ooa, ehg, eur, ana, yam, yam_migr, ncol = 2)

#' ## Define admixture events
#'
#' The way `spammr` implements admixture events is by calling the `admixture()`
#' function. This function has a very obvious interface which you will be able
#' to read from the example below without any issues.
#'
#' One thing to note, however, is the fact that it is crucial that the `from`
#' and `to` populations have some sort of overlapping spatial range in order to
#' simulate admixture. This is probably rather obvious, as populations can't mix
#' in space-time if they don't overlap at a given point in space-time.
#'
#' For example, if you look at the spatial boundaries plotted above, you'll see
#' that the European and African populations don't have any overlap in
#' population ranges. If we try to instruct `spammr` to simulate
#' admixture between them, we will get an error:

#+ eval = F
admixture(from = eur, to = afr, rate = 0.1, start = 20000, end = 15000)

#' ```
#' Error:
#' No overlap between population ranges of EUR and AFR at time 20000!
#' SLiM will freeze when running this model (it won't be able to satisfy spatial
#' requirements of migrant individuals). Please check the spatial maps at
#' the specified time point.
#' ```

#' The reason for this is a lack of spatial overlap between the two populations.
#' The error message instructs us to visually verify this, which can be done by
#' `spammr`'s `plot()` function and the optional parameter `pop_facets = F`
#' (which is set to `TRUE` by default):

plot(eur, afr, pop_facets = F)

#' Many models will have several admixture events, which we can collect in a
#' simple R list:

admixtures <- list(
  admixture(from = ana, to = eur, rate = 0.5, start = 8500, end = 7000),
  admixture(from = yam_migr, to = eur, rate = 0.7, start = 4000, end = 3000)
)

#' Note that the `admixture()` function returns nothing else than a data frame
#' collecting all the admixture parameters for the `compile()` step below:

admixtures

#' ## Compile the whole model and load it in SLiM

#' The most crucial function of `spammr` is `compile()`. It takes all population
#' ranges defined across space and time together with list of admixture events
#' (this is optional, of course, as some models won't include admixture), and
#' then proceeds by converting all vectorized spatial ranges to a raster bitmap
#' form. Furthermore, it compiles all information about split times, $N_e$
#' values, admixture directions, times and rates, to a series of tables. All of
#' that will be saved automatically in a dedicated directory in a format that is
#' understood by the backend SLiM script provided by `spammr` (more on that
#' below).

compile(
  populations = list(afr, ooa, ehg, eur, ana, yam, yam_migr),
  admixtures = admixtures,
  output_dir = "~/Desktop/test-model/",
  overwrite = TRUE
)

#' We can inspect the contents of the directory and see that it does, indeed,
#' contain all defined spatial maps (now PNG files, which is what SLiM
#' requires):

list.files("~/Desktop/test-model", pattern = "*.png")

#' And it also contains a series of tab-separated configuration tables:

list.files("~/Desktop/test-model", pattern = "*.tsv")

#' These tables contain summaries of the model parameters which we defined
#' graphically above, namely:
#'
#' - the table of population splits:
read.table("~/Desktop/test-model/splits.tsv", header = T)

#' - the table of admixture events:
read.table("~/Desktop/test-model/admixtures.tsv", header = T)

#' - and finally, the table of populations whose spatial maps will be updated
#' throughout the simulation, as well as the times of those updates:
read.table("~/Desktop/test-model/maps.tsv", header = T)

#' ## Running the simulation

#' The way we feed the entire serialized model into SLiM is through
#' the `run()` function, which understands the format of the model
#' directory created by the `compile()` function and generates a SLiM
#' script (using a backend skeleton script which is a part of this
#' package and can be found by calling `system.file("inst/extdata/backend.slim", package = "spammr")`).
#'
#' Note that when you run this model in SLiMgui (which should automatically open
#' by calling the command below), you will see populations pop up in individual
#' panels. This is how SLiMgui tracks spatial ranges of different populations.
#' _Everyone is still simulated in the same world_, it's just that the
#' simulation visualizes individual population ranges separately to reduce
#' clutter.

#+ eval = FALSE
run(
  model_dir = "~/Desktop/test-model/",
  gen_time = 30, burnin = 200, sim_length = 70000,
  interaction = 30, spread = 20, seq_length = 100, recomb_rate = 0
)

#' ## Animating the population movement
#'
#' As you could see when you ran the `spammr` model in SLiMgui, it can be a bit
#' hard to catch all that is going on visually during the simulation. To have a
#' better idea about what is going on, `spammr` provides an animation function,
#' which accepts one of the output files of the SLiM simulation backend script,
#' and renders the individual movement over time as a GIF animation.

#+ eval = FALSE
animate(
  locations = "~/Desktop/test-model/output_locations.tsv.gz",
  gif = "~/Desktop/test-model/output_anim.gif",
  gen_time = 30,
  nframes = 200
)
