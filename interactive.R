devtools::load_all(".")
library(plotly)

map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "naturalearth",
              crs = "EPSG:3035", "~/Google/postdoc/data/ne_data")

africa <- region(
  "Africa", map,
  coords = list(c(-18, 20), c(40, 20), c(30, 33),
                c(20, 32), c(10, 35), c(-8, 35))
)
europe_anatolia <- region(
  "Western Europe & Anatolia", map,
  coords = list(c(-10, 35), c(-5, 35), c(10, 38), c(20, 35), c(38, 35),
                c(40, 40), c(30, 45), c(20, 58), c(-5, 60), c(-15, 50))
)
europe <- region(
  "Western Europe", map,
  coords = list(c(-8, 35), c(-5, 36), c(10, 38), c(20, 35), c(25, 35),
                c(28, 45), c(20, 58), c(-5, 60), c(-15, 50))
)
anatolia <- region(
  "Anatolia", map,
  coords = list(c(28, 35), c(40, 35), c(42, 40),
                c(30, 43), c(27, 40), c(25, 38))
)

afr <- population(
  "AFR", parent = "ancestor", N = 2000, map = map,
  coords = list(c(-18, 20), c(40, 20), c(30, 33),
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
  coords = list(c(26, 55), c(38, 45), c(48, 53), c(60, 53),
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
  center = c(34, 38), radius = 500000, region = anatolia
) %>% expand(
  by = 20, start = 10000, end = 7000,
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

p <- plot(afr, ooa, ehg, eur, ana, yam, yam_migr)

x <- europe
ggplotly(ggplot()  + geom_sf(data = sf::st_cast(x, "MULTIPOLYGON")))

ggplotly(plot(afr, ooa))

admixtures <- list(
  admixture(from = ana, to = eur, rate = 0.5, start = 8000, end = 6000, overl = F),
  admixture(from = yam_migr, to = eur, rate = 0.75, start = 4000, end = 3000, overl = F)
)

model <- compile(
  populations = list(afr, ooa, ehg, eur, ana, yam, yam_migr),
  admixtures = admixtures,
  model_dir = "~/Desktop/demo-model", gen_time = 30, resolution = 10000,
  overwrite = T
)






iplot <- function(..., graticules = "original") {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

  args <- list(...)
  # extract the map component underlying each population object
  # and make sure they are all the same with no conflicts
  maps <- unique(lapply(args, function(i) attr(i, "map")))
  if (length(maps) != 1) {
    stop("Objects do not share the same map component", call. = F)
  }
  map <- maps[[1]]

  pops <- do.call(rbind, lapply(list(...), function(i) {
    if (nrow(map)) intersect_features(i) else i
  }))

  p_map <-  ggplot() + theme_bw()
  
  # plot the world map if a real geographic map was specified
  if (nrow(map))
    p_map <- p_map + geom_sf(data = sf::st_cast(map, "MULTIPOLYGON"),
                             fill = "lightgray", color = NA)

  if (graticules == "original" & has_crs(map))
    graticule_crs <- "EPSG:4326"
  else
    graticule_crs <- sf::st_crs(map)

  pops$pop <- factor(pops$pop)

  p_map <- p_map + geom_sf(data = sf::st_cast(pops, "MULTIPOLYGON"),
                           aes(fill = pop, frame = rev(time)), color = NA)

  if (has_crs(map))
    p_map <- p_map + coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0)
  else
    p_map <- p_map + coord_sf(
      xlim = attr(map, "xrange"),
      ylim = attr(map, "yrange"),
      expand = 0
    )

  plotly::ggplotly(p_map)
}
