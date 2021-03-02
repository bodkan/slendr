# define a section of the map of the world
world <- world_map(
  lon = c(-15, 60),  # min-max longitude
  lat = c(20, 65),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)

europe_anatolia <- region(
  "Western Europe & Anatolia",
  world,
  coords = list(
    c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
    c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50)
  )
)

europe <- region(
  "Western Europe",
  world,
  coords = list(
    c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
    c(28, 45), c(20, 58), c(-5, 60), c(-15, 50)
  )
)

anatolia <- region(
  "Anatolia",
  world,
  coords = list(
    c(28, 35), c(40, 35),
    c(42, 40), c(30, 43), c(27, 40), c(25, 38)
  )
)

plot(europe_anatolia, europe, anatolia)

whg <- population(
  name = "WHG",       # population identifier
  time = 25000,       # time in years ago
  world,              # world map 'context' for the population
  center = c(-1, 47), # (longitude, latitude)
  radius = 1300       # radius of a circle in km
)

plot(whg, rendering = F)
plot(whg)

whg <- population(
  name = "WHG",       # population identifier
  time = 25000,       # time in years ago
  world,              # world map 'context' for the population
  region = europe     # geographic boundary
)

plot(whg, rendering = F)
plot(whg)

ana <- population(
  name = "ANA",
  time = 9000,
  world,
  center = c(34, 38),
  radius = 700,
  region = anatolia
)

plot(ana, rendering = F)
plot(ana)

yam <- population(
  name = "YAM",
  time = 7000,
  world,
  coords = list(
    c(26, 50), c(38, 49), c(48, 50),
    c(48, 56), c(38, 59), c(26, 56)
  )
)

plot(yam, rendering = F)
plot(yam)

neol <- population(
  name= "NEOL",
  time = 7000,
  world,
  center = c(10, 48),
  radius = 800
)

plot(neol, rendering = F)
plot(neol)

yam_migr <- population(
  "YAM_migr",
  time = 5000,
  world,
  center = c(30, 52),
  radius = 200,
) %>%
  migrate(
    towards = c(10, 48),  # migrate towards this point
    duration = 1000,      # how many years does the migration take?
    snapshots = 15        # how many discrete snapshots should it take?
  )

plot(yam_migr, rendering = F)

ana_exp <- ana %>%
  expand(
    by = 2500,
    duration = 5000,
    snapshots = 10,
    region = europe_anatolia
  )

plot(ana_exp, rendering = F)
plot(ana_exp)

plot(whg, neol, yam, yam_migr, ana_exp)

plot(whg, ana, neol, yam_migr, ana_exp, facets = T, rendering = F)

plot(ana_exp)



# define a section of the map of the world
world <- world_map(
  lon = c(-15, 60),  # min-max longitude
  lat = c(20, 65),   # min-max latitude
  crs = "EPSG:3035"  # real projected CRS used internally
)

anatolia <- region(
  "Anatolia",
  world,
  coords = list(
    c(28, 35), c(40, 35),
    c(42, 40), c(30, 43), c(27, 40), c(25, 38)
  )
)

ana <- population(
  name = "ANA",
  time = 9000,
  world,
  center = c(34, 38),
  radius = 700,
  region = anatolia
)

pop <- render(ana)

pop_info <- data.frame(
  pop = pop$pop[1],
  time = pop$time[1]
)

ras <- stars::st_rasterize(pop)
ras_sf <- sf::st_as_sf(ras)
ras_sf$pop <- pop$pop
attr(ras_sf, "world") <- world
class(ras_sf) <- set_class(ras_sf, "pop")

bbox <- sf::st_bbox(world)
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = ras_sf, fill = "white") +
  coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)])

x <- yam
x@bbox <- sf::st_bbox(world)
plot(stars::st_rasterize(yam))

pdf("test1.pdf")
pop <- world
raster <- stars::st_rasterize(pop)
bbox <- sf::st_bbox(world)
plot(raster, xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)])
pop <- render(ana[2, ])
raster <- stars::st_rasterize(pop)
bbox <- sf::st_bbox(world)
plot(raster, xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)])
box(lty = '1373', col = 'red')
class(pop) <- set_class(pop, "pop")
plot(pop, geo_graticules = F) +
  geom_vline(xintercept = bbox[c(1, 3)]) +
  geom_hline(yintercept = bbox[c(2, 4)])
dev.off()



pop <- render(ana[2, ])
pop <- world
raster <- stars::st_rasterize(pop)
bbox <- sf::st_bbox(world)
plot(raster, xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)])

pop <- render(ana[2, ])
raster <- stars::st_rasterize(pop)
bbox <- sf::st_bbox(world)
plot(sf::st_crop(raster, box), xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)])

sf::st_crop(raster, box)

class(pop) <- set_class(pop, "pop")
plot(pop, geo_graticules = F) +
  geom_vline(xintercept = bbox[c(1, 3)]) +
  geom_hline(yintercept = bbox[c(2, 4)])
