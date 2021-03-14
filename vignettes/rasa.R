devtools::load_all("~/projects/spammr")

world <- world_map(
  xrange = c(-15, 70),  # min-max longitude
  yrange = c(20, 75),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)
plot(world)

eurasia <- region("Eurasia", world, coords = list(
  c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(35, 30),
  c(38, 20), c(50, 15), c(60, 15), c(70, 20), c(80, 30),
  c(90, 40), c(90, 60), c(50, 70), c(40, 70), c(0, 70), c(-15, 50)
)); plot(eurasia)

pop <- population(
  "pop", parent = "ancestor", Ne = 10000,
  world = world, region = eurasia
)

plot(pop)

compile(pop, output_dir = "rasa", overwrite = TRUE)

run_slimgui("rasa/", gen_time = 30,burnin = 2000, seq_length = 100,
            recomb_rate = 0, sim_length = 20000, interaction = 30, spread = 10)
