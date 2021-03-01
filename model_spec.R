# define a section of the map of the world
world <- world_map(
  lon = c(-15, 60),  # min-max longitude
  lat = c(20, 65),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)

# boundary of West Eurasia
west_eurasia <- region(
  "West Eurasia",
  world,
  coords = list(
    c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
    c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50)
  )
)

anatolia <- region(
  "Anatolia",
  world,
  coords = list(
    c(28, 35), c(45, 35),
    c(46, 40), c(30, 43), c(27, 40), c(25, 38)
  )
)

whg <- population(
  name = "WHG",       # population identifier
  time = 25000,       # time in years ago
  world,              # world map 'context' for the population
  center = c(-1, 47), # (longitude, latitude)
  radius = 1300,      # radius of a circle in km
)

ana <- population(
  name = "ANA",
  time = 9000,
  world,
  center = c(34, 38),
  radius = 600
)

yam <- population(
  name = "YAM",
  time = 7000,
  world,
  coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  )
)

neol <- population(
  name= "NEOL",
  time = 7000,
  world,
  center = c(-1, 47),
  radius = 800
)

yam_migr <- population(
  "YAM_migr",
  time = 5000,
  world,
  center = c(30, 52),
  radius = 200,
) %>%
  migrate(
    lon = 9, lat = 48, # migrate towards this point
    duration = 1000,   # how many years does the migration take?
    snapshots = 10     # how many discrete snapshots should it take?
  )

plot(whg, ana, neol, yam_migr)
plot(whg, ana, neol, yam_migr, snapshots = T)


ana_exp <- expand(ana, by = 5000, duration = 5000, snapshots = 10)

plot(ana_exp)

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
