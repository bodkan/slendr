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
  name = "NEOL", time = 1000, Ne = 300, parent = afr,
  world, region = europe
)

yam <- population(
  name = "YAM", time = 800, Ne = 600, parent = afr,
  world, coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  )
)

yam_migr <- population(
  name = "YAM_migr", parent = yam, time = 700, Ne = 600,
  center = c(42, 50), radius = 200
) %>%
  migrate(
    trajectory = list(c(28, 45), c(21, 36)),
    duration = 200,
    snapshots = 20
  )
plot(yam_migr)

plot(afr, neol, yam_migr)
plot(afr, neol, yam_migr, pop_facets = F)

admixtures <- list(
  admixture(from = yam_migr, to = neol, rate = 0.1,  start = 500, end = 510)
)

compile(
  afr, neol, yam, yam_migr,
  output_dir = "admixture/",
  admixtures = admixtures,
  overwrite = TRUE
)

run_slimgui(model_dir = "admixture/", gen_time = 1, burnin = 10,
            sim_length = 1000, seq_length = 100, interaction = 20,
            spread = 10, recomb_rate = 0)

