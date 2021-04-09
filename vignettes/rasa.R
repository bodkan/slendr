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

compile(pop, output_dir = "~/Desktop/europe", resolution = 10, overwrite = TRUE)

run("~/Desktop/europe/", gen_time = 1, burnin = 1, seq_length = 1,
    recomb_rate = 0, sim_length = 100,
    max_distance = 50, max_spread = 5)

sel_pos <- c(428, 319)
sel_pos <- c(680, 220)

run("~/Desktop/europe/", gen_time = 1, burnin = 1, seq_length = 1,
    recomb_rate = 0, sim_length = 100,
    max_distance = 50, max_spread = 5,
    sel_freq = 0.1, sel_coef = 0.5, sel_time = 10,
    sel_pop = "pop", sel_x = sel_pos[1], sel_y = sel_pos[2],
    include = "~/projects/spammr/inst/extdata/selection.slim")
