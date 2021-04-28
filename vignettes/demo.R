devtools::load_all(".")

world <- map(xrange = c(-15, 60), yrange = c(20, 65), crs = "EPSG:3035")

africa <- region(
  "Africa", world,
  coords = list(c(-18, 20), c(40, 20), c(30, 33),
                c(20, 32), c(10, 35), c(-8, 35))
)

europe_anatolia <- region(
  "Western Europe & Anatolia", world,
  coords = list(c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
                c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50))
)

europe <- region("Western Europe", world,
  coords = list(c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
                c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)

anatolia <- region(
  "Anatolia", world,
  coords = list(c(28, 35), c(40, 35), c(42, 40),
                c(30, 43), c(27, 40), c(25, 38))
)

afr <- population("AFR", parent = "ancestor", N = 2000, world = world, region = africa)

ooa <- population(
  "OOA", parent = afr, time = 51000, N = 200, remove = 27000,
  center = c(33, 30), radius = 500
) %>% move(
  trajectory = list(c(40, 30), c(50, 30), c(60, 40), c(70, 40)),
  start = 50000, end = 40000, snapshots = 30
)

ehg <- population(
  "EHG", time = 28000, N = 400, parent = ooa, remove = 6000,
  world, coords = list(c(26, 55), c(38, 53), c(48, 53), c(60, 53),
                       c(60, 60), c(48, 63), c(38, 63), c(26, 60))
)

eur <- population(name = "EUR", time = 25000, N = 1000, parent = ehg, world, region = europe)

ana <- population(
  name = "ANA", time = 28000, N = 800, parent = ooa, remove = 6000,
  world, center = c(34, 38), radius = 700, region = anatolia
) %>% expand(
  by = 2500, start = 10000, end = 7000, snapshots = 10, region = europe_anatolia)

yam <- population(
  name = "YAM", time = 7000, N = 600, parent = ehg, remove = 2000
  world, coords = list(c(26, 50), c(38, 49), c(48, 50),
                       c(48, 56), c(38, 59), c(26, 56))
)

yam_migr <- population(
  name = "YAM_migr", time = 6000, N = 1000, parent = yam, remove = 2900,
  world, coords = list(c(26, 50), c(38, 49), c(48, 50),
                       c(48, 56), c(38, 59), c(26, 56)
) %>%
  move(trajectory = c(15, 50), start = 5000, end = 3000, snapshots = 8)

admixtures <- list(
  admixture(from = ana, to = eur, rate = 0.5, start = 8000, end = 6000),
  admixture(from = yam_migr, to = eur, rate = 0.75, start = 4000, end = 3000)
)


model <- compile(
  populations = list(afr, ooa, ehg, eur, ana, yam, yam_migr),
  admixtures = admixtures,
  model_dir = "~/Desktop/demo-model", gen_time = 30, resolution = 10
)

graph(model)

run(
  model, sim_length = 52000,
  seq_length = 1, recomb_rate = 0,
  max_interaction = 200, spread = 50,
  save_locations = T, track_ancestry = F,
  how = "batch"
)


animate(model, nframes = 200)
