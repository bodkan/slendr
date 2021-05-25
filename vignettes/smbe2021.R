devtools::load_all(".")

eurasia <- world(
  xrange = c(-15, 60), yrange = c(20, 65),
  landscape = "naturalearth", crs = "EPSG:3035"
)

eur <- population(
  name = "EUR", time = 10000, N = 5000, map = eurasia,
  polygon = list(c(-8, 35), c(10, 38), c(20, 35), c(25, 35),
                 c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)

ehg <- population(
  "EHG", time = 10000, N = 4000, map = eurasia,
  polygon = list(c(26, 55), c(38, 53), c(48, 53), c(60, 53),
                 c(60, 60), c(48, 63), c(38, 63), c(26, 60))
)

ana <- population(
  name = "ANA", time = 10000, N = 0800, map = eurasia,
  polygon = list(c(28, 35), c(40, 35), c(42, 40),
                 c(30, 43), c(27, 40), c(25, 38))
) %>%
  expand(
    by = 2500e3, start = 10000, end = 7000,
    snapshots = 10
  )

yam <- population(
  name = "YAM", time = 7000, N = 3000, parent = ehg,
  polygon = list(c(26, 50), c(38, 49), c(48, 50),
                 c(48, 56), c(38, 59), c(26, 56))
) %>%
  move(trajectory = c(15, 50), start = 5000, end = 3000, snapshots = 8)

geneflows <- list(
  geneflow(ana, yam, start = 6500, end = 6000, rate = 0.3),
  geneflow(ana, eur, start = 8000, end = 6000, rate = 0.5),
  geneflow(yam, eur, start = 4000, end = 3000, rate = 0.75)
)

model <- compile(
  populations = list(ehg, ana, yam, eur), geneflows = geneflows,
  generation_time = 30, resolution = 10e3,
  competition_dist = 200e3, mate_dist = 200e3, offspring_dist = 100e3,
  dir = "/tmp/test-model", overwrite = TRUE
)

graph(model)

explore(model)

slim(
  model, seq_length = 1, recomb_rate = 0,
  save_locations = F, track_ancestry = F,
  method = "gui"
)
