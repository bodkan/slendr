devtools::load_all("~/projects/spammr")


# simulation of Europe

world <- map(
  xrange = c(-15, 70),  # min-max longitude
  yrange = c(20, 75),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)
plot(world)

eurasia <- region("Eurasia", world, coords = list(
  c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(35, 30),
  c(60, 30), c(70, 40), c(80, 50), c(50, 70), c(40, 70),
  c(0, 70), c(-15, 50)
))
plot(eurasia)

pop <- population(
  "pop", parent = "ancestor", Ne = 20000,
  world = world, region = eurasia
)
plot(pop)

compile(pop, output_dir = "~/Desktop/europe", pixel = 10000, overwrite = TRUE)

run("~/Desktop/europe/", gen_time = 1, burnin = 1, seq_length = 100,
    recomb_rate = 0, sim_length = 10000,
    interaction = 70, max_interaction = 100, spread = 5)

# run("~/Desktop/europe/", gen_time = 1, burnin = 1, seq_length = 100,
#     recomb_rate = 0, sim_length = 1000,
#     competition = 50, mate_choice = 50, spread = 5,
#     frequency = 0.1, sel_x = 428, sel_y = 319, sel_coef = 0.1, sel_time = 2,
#     include = "~/projects/spammr/inst/extdata/selection.slim")

# "blank" simulation

world <- map(
  xrange = c(0, 5),  # min-max longitude
  yrange = c(44, 47),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)
plot(world)

pop <- population(
  "pop", parent = "ancestor", Ne = 20000,
  world = world, coords = list(c(0, 44), c(5, 44.2), c(5, 47), c(0, 47))
)
plot(pop)

compile(pop, output_dir = "~/Desktop/blank", pixel = 500, overwrite = TRUE)

run("~/Desktop/blank/", gen_time = 1, burnin = 1, seq_length = 100,
    recomb_rate = 0, sim_length = 10000,
    interaction = 70, max_interaction = 100, spread = 5)
