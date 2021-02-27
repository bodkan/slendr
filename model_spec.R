devtools::load_all(".")

## map of the world
world <- make_world(
  lon = c(-15, 60),
  lat = c(20, 65),
  target_crs = "EPSG:3035"
)

## boundary of West Eurasia
region_weur <- make_region(
  "West Eurasia",
  coords = list(
    c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
    c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50)
  ),
  world
)

whg <- circ_range(
  pop = "WHG",
  time = 25000,
  lon = -1, lat = 47,
  radius = 1300,
  world
)

ana <- circ_range(
  pop = "ANA",
  time = 9000,
  lon = 34, lat = 38,
  radius = 600,
  world
)

yam <- poly_range(
  pop = "YAM",
  time = 7000,
  coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  ),
  world
)

neol <- circ_range(
  pop = "NEOL",
  time = 7000,
  lon = -1, lat = 47,
  radius = 800,
  world
)

yamigr <- circ_range(
  "YAM_migr",
  lon = 30, lat = 52,
  time = 5000, radius = 200,
  world
) %>%
  migrate(
    lon = 9, lat = 48,
    duration = 1000,
    nslices = 4
  )

anaexp <- expand(ana, by = 5000, duration = 5000, snapshots = 10)

plot_ranges(world, europe)

plot_ranges(world)
plot_ranges(world, whg)
plot_ranges(world, ana)
plot_ranges(world, neol)
plot_ranges(world, yam)
plot_ranges(world, yamigr)
plot_ranges(world, anaexp)

whg2 <- render_ranges(whg, world)
ana2 <- render_ranges(ana, world)
neol2 <- render_ranges(neol, world)
yam2 <- render_ranges(yam, world)
yamigr2 <- render_ranges(yamigr, world)
anaexp2 <- render_ranges(anaexp, world, region_weur)

plot_ranges(world)
plot_ranges(world, whg2)
plot_ranges(world, ana2)
plot_ranges(world, neol2)
plot_ranges(world, yam2)
plot_ranges(world, yamigr2)
plot_ranges(world, anaexp2)

plot_ranges(world, whg, ana, neol, yam, yamigr, anaexp)
plot_ranges(world, whg2, ana2, neol2, yam2, yamigr2, anaexp2) + facet_wrap(~ pop)



plot_ranges(world, anamigr2)

x <- anamigr2
anamigr2@bbox <- st_bbox(world)
plot(st_rasterize(anamigr2))
