# always a good idea to first fetch the very latest dev version of the package
# because the software is updated every couple of hours
devtools::install_github("bodkan/spammr")

library(spammr)

world <- map(
  xrange = c(-15, 70), # min-max longitude
  yrange = c(20, 75),  # min-max latitude
  crs = "EPSG:3035"    # real projected CRS used internally
)

# restrict individual movement only to this regions
eurasia <- region("Eurasia", world, coords = list(
  c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(35, 30),
  c(60, 30), c(70, 40), c(80, 50), c(50, 70), c(40, 70),
  c(0, 70), c(-15, 50)
))

# create a single population
pop <- population(
  "pop", parent = "ancestor", Ne = 10000,
  world = world, region = eurasia
)
plot(pop)

# compile the model specification and the single spatial map
model <- compile(
  pop,
  model_dir = "~/Desktop/europe/",
  resolution = 10,
  gen_time = 30,
  overwrite = TRUE
)

# neutral model for testing the uniformity of the spread
#   - max_distance is the parameter used in:
#     initializeInteractionType(1, "xy", reciprocal=T, maxDistance=<<here>>)
#     calls and is discussed in the SLiM manual (it influences the maximum
#     spatial competition distance and mating choice distance between individuals)
#   - max_spread
# both parameters in units of the raster size (i.e. pixels, same as SLiM)
run(
  model, burnin = 30, sim_length = 30000,
  recomb_rate = 0, seq_length = 100,
  max_distance = 50, max_spread = 5
)

# substitute parameters into the selection script template
selection_script <- script(
  system.file("extdata", "selection.slim", package = "spammr"),
  s = 0.5, # selection coefficient of an additive
  freq = 0.1, onset = 29000,
  origin = "pop",
  coord = convert(lat = 10, lon = 45, model)
)

# run the demographic model, including the selection component
run(
  model, burnin = 30, sim_length = 30000,
  recomb_rate = 0, seq_length = 100,
  max_distance = 50, max_spread = 5,
  include = selection_script
)
