
devtools::load_all(".")

map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "naturalearth", crs = "EPSG:3035")

africa <- region(
  "Africa", map,
  polygon = list(c(-18, 20), c(40, 20), c(30, 33),
                 c(20, 32), c(10, 35), c(-8, 35))
)
europe_anatolia <- region(
  "Western Europe & Anatolia", map,
  polygon = list(c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
                 c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50))
)
europe <- region(
  "Western Europe", map,
  polygon = list(c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
                 c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)
anatolia <- region(
  "Anatolia", map,
  polygon = list(c(28, 35), c(40, 35), c(42, 40),
                 c(30, 43), c(27, 40), c(25, 38))
)

afr <- population(
  "AFR", parent = "ancestor", time = 60000, N = 2000, map = map,
  polygon = list(c(-18, 20), c(40, 20), c(30, 33),
                 c(20, 32), c(10, 35), c(-8, 35))
)

ooa <- population(
  "OOA", parent = afr, time = 51000, N = 200, remove = 27000,
  center = c(33, 30), radius = 500000
) %>% move(
  trajectory = list(c(40, 30), c(50, 30), c(60, 40)),
  start = 50000, end = 40000, snapshots = 30
)
ehg <- population(
  "EHG", time = 28000, N = 400, parent = ooa, remove = 6000,
  coords = list(c(26, 55), c(38, 53), c(48, 53), c(60, 53),
                c(60, 60), c(48, 63), c(38, 63), c(26, 60))
)
eur <- population(
  name = "EUR",
  time = 25000,
  N = 1000,
  parent = ehg,
  region = europe
)
ana <- population(
  name = "ANA", time = 28000, N = 800, parent = ooa, remove = 6000,
  center = c(34, 38), radius = 500e3, region = anatolia
) %>% expand(
  by = 2000e3, start = 10000, end = 7000,
  snapshots = 10,
  region = europe_anatolia
)
yam <- population(
  name = "YAM", time = 7000, N = 600, parent = ehg, remove = 2000,
  coords = list(c(26, 50), c(38, 49), c(48, 50),
                c(48, 56), c(38, 59), c(26, 56))
)
yam_migr <- population(
  name = "YAM_migr", time = 6000, N = 1000, parent = yam, remove = 2900,
  coords = list(c(26, 50), c(38, 49), c(48, 50),
                c(48, 56), c(38, 59), c(26, 56))
) %>%
  move(trajectory = c(15, 50), start = 5000, end = 3000, snapshots = 8)

admixtures <- list(
  admixture(from = ana, to = eur, rate = 0.5, start = 8000, end = 6000),
  admixture(from = yam_migr, to = eur, rate = 0.75, start = 4000, end = 3000)
)

model <- compile(
  populations = list(afr, ooa, ehg, eur, ana, yam, yam_migr),
  admixtures = admixtures,
  model_dir = "/tmp/demo-model", generation_time = 30, resolution = 10e3,
  overwrite = T
)

explore(model)

slim(
  model, seq_length = 1, recomb_rate = 0,
  max_interaction = 100e3, spread = 50e3,
  save_locations = T, track_ancestry = F,
  method = "gui"
)
