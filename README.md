<!-- README.md is generated from README.Rmd. Edit that file instead. -->



## spammr: *S*patiotemporal *P*opulation *A*dmixture and *M*igration *M*odels in *R*

### ⚠️⚠️⚠️ WARNING ⚠️⚠️⚠️

**This software is still under development!**

If you are interested in realistic spatio-temporal population genetic
simulations and somehow stumbled upon this repository then
congratulations! Your google-fu is strong.

I have been making good progress towards a first beta version, but the
package still has some way to go before being production ready.

That said, if you would like to learn more, or if you're feeling brave
and would like to test the package yourself, take a look at the
[tutorial](https://bodkan.net/spammr/articles/tutorial.html). Note
that getting it installed with all the geospatial dependencies can be
a little bit tricky at this point (see the relevant section in the
tutorial). If loading the package fails, check the error messages for
missing software and install it using the package manager of your
choice (on a Mac I recommend _homebrew_).

If you would like to stay updated with the developments:

1. Click on the "Watch" button on the project's [Github
website](https://www.github.com/bodkan/spammr).

2. Follow me on [Twitter](https://www.twitter.com/fleventy5). I might
post some updates once the software is a bit more ready.

### Installation

For installation instructions, please take a look at the installation
section [of the
tutorial](https://bodkan.net/spammr/articles/tutorial.html#installation-and-setup-1). Note
that you might need to install some non-R software dependencies
first. At the very least, you will need the most recent version of the
[SLiM software](https://messerlab.org/slim/) (version 3.6).

### Example

Here is a small demonstration of what *spammr* is designed to do. Note that this model is not supposed to recapitulate the history of any known species and serves just for demonstration purposes.

For a more complete example that is less abstract and demonstrates the usage of the package on a model of West Eurasian history, please see the [tutorial](https://bodkan.net/spammr/articles/tutorial.html).

We're going to simulate a demographic history of several populations,
including splits and admixture events, defining the spatial boundaries
of populations as well as their changing dynamics over time (steps
1-6). We will then feed model defined in R into SLiM and instruct it
(using a back-end SLiM script provided by this package) to simulate
data based on the model specification (step 7). Finally, we will
generate some visualizations to make sure that the simulation
proceeded as we wanted to (steps 8 and 9).

#### 1. Setup the spatial context ("the world" occupied by populations)




```r
library(spammr)

world <- map(
  xrange = c(-15, 60), # min-max longitude
  yrange = c(20, 65),  # min-max latitude
  crs = "EPSG:3035"    # real projected CRS used internally
)
```

#### 2. Define demographic history and population boundaries

Times are given in "units before present", distances in kilometers.


```r
p1 <- population(
  name = "pop1", parent = "ancestor", N = 700,
  radius = 600,       # radius of the circular range
  center = c(10, 25), # latitude, longitude coordinates of the center
  world = world
)

p2 <- population(
  name = "pop2", parent = p1, time = 30000, N = 500,
  center = c(10, 25), radius = 300
) %>%
  move(
    trajectory = list(c(25, 25), c(40, 30), # trajectory of movement
                      c(40, 40), c(50, 50)),
    start = 29000, end = 25000, snapshots = 30
  )

p3 <- population(
  name = "pop3", parent = p2, time = 20000, N = 2000,
  coords = list(c(-10, 50), c(10, 50), c(20, 53), # polygon range
                c(40, 60), c(40, 70), c(-10, 65))
)

p4 <- population(
  name = "pop4", parent = p2, time = 15000, N = 2000,
  coords = list(c(-10, 35), c(20, 37), c(25, 40),
                c(30, 45), c(10, 50), c(-10, 45))
)

p5 <- population(
  name = "pop5", parent = p1, time = 10000, N = 3000,
  center = c(10, 25), radius = 300
) %>%
  move( # population migration...
    trajectory = list(c(-5, 33), c(-5, 40)),
    start = 9000, end = 8000,
    snapshots = 20
  ) %>% # ... followed by expansion
  expand(by = 2000, start = 7000, end = 2000, snapshots = 10)
```

#### 3. Visualize the spatial maps of each population


```r
plot(p1, p2, p3, p4, p5, ncol = 2)
```

![plot of chunk unnamed-chunk-5](man/figures/README-unnamed-chunk-5-1.png)

#### 4. Define admixture events


```r
admixtures <- list(
  admixture(from = p5, to = p4, rate = 0.25, start = 2000, end = 0),
  admixture(from = p5, to = p3, rate = 0.6, start = 2000, end = 0)
)
```

#### 5. Compile the model to a set of configuration files


```r
model <- compile(
  model_dir = "/tmp/example-model", # location of serialized model data
  populations = list(p1, p2, p3, p4, p5),
  admixtures = admixtures,
  gen_time = 30,
  resolution = 10  # how many km per pixel?
)
```

Compiled model is kept as an R object which can be passed to different
functions, most importantly the `run()` function shown below.


```r
model
#> spammr 'model' object 
#> --------------------- 
#> populations: pop1, pop2, pop3, pop4, pop5 
#> admixture events: 2 
#> generation time: 30 
#> number of spatial maps: 71 
#> resolution: 10 km per pixel
#> 
#> configuration files in: /tmp/example-model 
#> 
#> For detailed model specification see `$splits`, `$admixtures`, `$maps`,
#> or `$populations` components of the model object, or the configuration
#> files in the model directory.
```

#### 6. Visualize the implied admixture graph


```r
graph(model)
```

![plot of chunk unnamed-chunk-9](man/figures/README-unnamed-chunk-9-1.png)

#### 7. Run the model in SLiM (in batch mode in this case)




```r
run(
  model,
  burnin = 1000, sim_length = 31000,
  seq_length = 1, recomb_rate = 0, track_ancestry = 1, # single locus
  save_locations = TRUE,
  interaction = 200, spread = 50, # interaction and spread (in km)
  how = "gui" # open the model in SLiMgui
)
```

As specified, the SLiM run will save ancestry proportions in each
population over time as well as the location of every individual who
ever lived.

#### 8. Verify the simulated ancestry proportions


```r
ancestries(model)
```

![plot of chunk unnamed-chunk-12](man/figures/README-unnamed-chunk-12-1.png)

#### 9. Re-capitulate the SLiM run as an individual-based animation


```r
animate(model, nframes = 200)
```

![plot of chunk unnamed-chunk-13](man/figures/README-unnamed-chunk-13-1.gif)

Note that it is possible to simulate population splits and admixture
both by "physically" moving individuals of a population from one
destination to the next across space but it is also possible to do
this more abstractly (in instantaneous "jumps") in situations where
this is more appropriate or where simulating accurate movement is not
necessary.

In this case, we simulated the split of populations _pop3_ and _pop4_
from _pop2_ instantaneously, without explicitly tracing their
movement. Compare this to the split of _pop5_ from _pop1_, where we
defined the population movement explicitly.
