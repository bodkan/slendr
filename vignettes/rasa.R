devtools::load_all("~/projects/spammr")

world <- map(
  xrange = c(-15, 70),  # min-max longitude
  yrange = c(20, 75),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)

eurasia <- region("Eurasia", world, coords = list(
  c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(35, 30),
  c(60, 30), c(70, 40), c(80, 50), c(50, 70), c(40, 70),
  c(0, 70), c(-15, 50)
))

pop <- population(
  "pop", parent = "ancestor", Ne = 10000,
  world = world, region = eurasia
)
plot(pop)

model <- compile(
  pop,
  model_dir = "~/Desktop/europe/",
  resolution = 10,
  gen_time = 30,
  overwrite = TRUE
)

run(
  model,
  burnin = 100, sim_length = 30000,
  recomb_rate = 0, seq_length = 100,
  max_distance = 50, max_spread = 5
)




# optional scripts?

# direct path
script1 <- "~/my/path/to/a/custom/selection/script.slim"

run(
  model,
  burnin = 100, sim_length = 30000,
  recomb_rate = 0, seq_length = 100,
  max_distance = 50, max_spread = 5,
  include = "~/projects/spammr/inst/extdata/selection.slim"
)

# script template
script2 <- script(
  "~/my/path/to/a/custom/selection/script.slim",
  sel_coef = 0.5, dominance = 0.5,
  init_freq = 0.1, time = 10,
  origin = "pop",
  coord = coordinate(lat = 10, lon = 45, world)
)

script3 <- script("~/custom/script/for/output/logging.slim", start = 30000, end = 0)

# include in the run
run(
  model,
  burnin = 100, seq_length = 3000,
  recomb_rate = 0, sim_length = 100,
  max_distance = 50, max_spread = 5,
  include = c(script1, script2, script3)
)
