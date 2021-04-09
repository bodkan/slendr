
## spammr: *S*patiotemporal *P*opulation *A*dmixture and *M*igration *M*odels in *R*

### ⚠️⚠️⚠️ WARNING ⚠️⚠️⚠️

**This software is still under heavy development!**

If you are interested in realistic spatio-temporal population genetic
simulations and somehow stumbled upon this repository then
congratulations! Your google-fu is strong.

This repository is public mostly so that I can share the results of my
work with collaborators without too much hassle. I have been making good
progress towards a first beta version, but the package still has some
way to go before being production ready.

That said, if you would like to learn more, or if you’re feeling brave
and would like to test the package yourself, take a look at the
[tutorial](https://bodkan.net/spammr/articles/tutorial.html). Note that
getting it installed with all the geospatial dependencies can be a
little bit tricky at this point (see the relevant section in the
tutorial). If loading the package fails, check the error messages for
missing software and install it using the package manager of your choice
(on a Mac I recommend *homebrew*).

If you would like to stay updated with the developments:

1.  Click on the “Watch” button on the project’s [Github
    website](https://www.github.com/bodkan/spammr).

2.  Follow me on [Twitter](https://www.twitter.com/fleventy5). I might
    post some updates once the software is a bit more ready.

### Installation

For installation instructions, please take a look at the installation
section [of the
tutorial](https://bodkan.net/spammr/articles/tutorial.html#installation-and-setup-1).
Note that you might need to install some additional non-R software
dependencies first.

### Example

Here is a very brief visual example of what *spammr* is designed to do.
We want to simulate a demographic history of several populations,
including splits and admixture events, defining spatial population
boundaries and their dynamics over time, feeding the spatial model
definition into SLiM and instructing it to simulate data according to
the spatio-temporal configuration in R.

1.  Setup the simulation context (“the world”):

``` r
library(spammr)

world <- map(
  xrange = c(-15, 60), # min-max longitude
  yrange = c(20, 65),  # min-max latitude
  crs = "EPSG:3035"    # real projected CRS used internally
)
```

2.  Define population splits and spatial boundaries (times in years ago,
    distances in kilometers):

``` r
p1 <- population(
  name = "pop1", parent = "ancestor", Ne = 1000,
  center = c(10, 25), # (latitude, longitude)
  radius = 600,
  world = world
)

p2 <- population(
  name = "pop2", parent = p1, time = 30000, Ne = 100,
  center = c(10, 25), radius = 300
) %>%
  move(
    trajectory = list(c(25, 25), c(40, 30), c(40, 40), c(50, 50)),
    start = 29000, end = 25000, snapshots = 20
  )

p3 <- population(
  name = "pop3", parent = p2, time = 20000, Ne = 3000,
  coords = list(c(-10, 50), c(10, 50), c(20, 53), # trajectory "checkpoints"
                c(40, 60), c(40, 70), c(-10, 65))
)

p4 <- population(
  name = "pop4", parent = p2, time = 15000, Ne = 3000,
  coords = list(c(-10, 35), c(20, 37), c(25, 40),
                c(30, 45), c(10, 50), c(-10, 45))
)

# expand the ancestral population further
p5 <- population(
  name = "pop5", parent = p1, time = 700, Ne = 100,
  center = c(10, 25), radius = 300
) %>%
  move(
    trajectory = list(c(-5, 33), c(-5, 40)),
    start = 10000, end = 8000,
    snapshots = 20
  ) %>%
  expand(by = 2000, start = 8000, end = 2000, snapshots = 10)
```

3.  Visualize the (still vectorized) spatial maps:

``` r
plot(p1, p2, p3, p4, p5)
```

4.  Define admixture between populations:

``` r
admixtures <- list(
  admixture(from = p5, to = p4, rate = 0.2, start = 2000, end = 0),
  admixture(from = p5, to = p3, rate = 0.2, start = 2000, end = 0)
)
```

5.  Compile the model:

``` r
unlink("/tmp/example-model", recursive = TRUE, force = TRUE)
model <- compile(
  model_dir = "/tmp/example-model",  # where to put all compiled data
  populations = list(p1, p2, p3, p4, p5),
  admixtures = admixtures,
  gen_time = 30,
  resolution = 10  # how many km per pixel?
)
```

6.  Visualize the admixture graph arising from our model:

``` r
graph(model)
```

7.  Run the model in SLiM in batch mode using a built-in backend script:

``` r
run(
  model,
  burnin = 1000,
  sim_length = 35000, # total length of the simulation
  seq_length = 1, recomb_rate = 0, track_ancestry = 1, # simulate just a single locus
  max_distance = 30, max_spread = 30
)
```
