devtools::load_all("~/projects/spammr")

world <- world_map(
  xrange = c(-15, 60),  # min-max longitude
  yrange = c(20, 65),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)

africa <- region(
  "Africa", world,
  coords = list(
    c(-18, 20), c(40, 20), c(30, 33),
    c(20, 32), c(10, 35), c(-8, 35)
  )
)

europe <- region(
  "Western Europe", world,
  coords = list(c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
                c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)

afr <- population(
  "AFR", parent = "ancestor", Ne = 1000,
  world = world, region = africa
)

neol <- population(
  name = "NEOL", time = 25000, Ne = 300, parent = afr,
  world, region = europe
)

yam <- population(
  name = "YAM", time = 7000, Ne = 600, parent = afr,
  world, coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  )
) %>%
  migrate(
    trajectory = c(10, 48),
    duration = 1000,
    snapshots = 8
  )

plot(afr, neol, yam)

compile(
  afr, neol, yam,
  output_dir = "model/",
  overwrite = TRUE
)

run_slimgui(model_dir = "model/", gen_time = 30, burnin = 200, sim_length = 70000)