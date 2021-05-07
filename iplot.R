library(plotly)

devtools::load_all(".")

map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "naturalearth",
              crs = "EPSG:3035")

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

admixtures <- list(
  admixture(from = ana, to = eur, rate = 0.5, start = 8000, end = 6000, overl = F),
  admixture(from = yam_migr, to = eur, rate = 0.75, start = 4000, end = 3000, overl = F)
)

model <- compile(
  populations = list(afr, ooa, ehg, eur, ana, yam, yam_migr),
  admixtures = admixtures,
  model_dir = "~/Desktop/demo-model", generation_time = 30, resolution = 10000,
  overwrite = T
)

plot(afr, eur, ooa, ehg, ana, yam, yam_migr)

graticules = "original"
pops = list(afr, eur, ooa, ehg, ana, yam, yam_migr)
names(pops) <- sapply(pops, function(i) i$pop[1])

## iplot <- function(..., graticules = "original") {
  if (!graticules %in% c("internal", "original"))
    stop("Graticules can be either 'original' or 'internal'", call. = FALSE)

## args <- list(...)
  # extract the map component underlying each population object
  # and make sure they are all the same with no conflicts
  maps <- unique(lapply(pops, function(i) attr(i, "map")))
  if (length(maps) != 1) {
    stop("Objects do not share the same map component", call. = F)
  }
  map <- maps[[1]]

  # plot the world map (if a real geographic map was specified)
  if (nrow(map)) {
    p_map <- geom_sf(data = sf::st_cast(map, "MULTIPOLYGON"),
                     aes(frame = NULL), fill = "lightgray", color = NA)
  } else {
    p_map <- NULL
  }

  if (graticules == "original" & has_crs(map)) {
    graticule_crs <- "EPSG:4326"
  } else {
    graticule_crs <- sf::st_crs(map)
  }

  if (has_crs(map)) {
    p_coord <- coord_sf(crs = sf::st_crs(map), datum = graticule_crs, expand = 0)
  } else {
    p_coord <- coord_sf(
      xlim = attr(map, "xrange"),
      ylim = attr(map, "yrange"),
      expand = 0
    )
  }

stop("asd")

removal_times <- sapply(names(pops), function(pop_name) {
  pop_maps <- pops[[pop_name]]
  attr(pops[[pop_name]], "remove")
})

# get times of all spatial maps across all populations
all_times <- unique(sort(c(
  0,
  removal_times,
  unlist(sapply(pops, function(i) i$time))
))) %>% .[. != Inf & . != -1]

all_maps <- lapply(names(pops), function(pop_name) {

  pop_maps <- pops[[pop_name]]

  # get times where the spatial map of the current population
  # needs to be filled in
  missing_times <- all_times[
    all_times >= removal_times[pop_name] &
        !all_times %in% pop_maps$time
  ]

  # generate the missing maps
  new_maps <- lapply(missing_times, function(t) {
    # get all preceding maps
    previous_maps <- pop_maps %>% .[.$time > t, ] # what's going on here with 2 maps?
    if (!nrow(previous_maps)) return(NULL)
    latest_map <- previous_maps[nrow(previous_maps), ]
    latest_map$time <- t
    latest_map
  }) %>%
    do.call(rbind, .)

  combined_maps <-
    rbind(pop_maps, new_maps) %>%
    .[order(-.$time), ] %>%
    .[.$time != Inf, ]

  attributes(combined_maps) <- attributes(pop_maps)

  combined_maps

})

# intersect the spatial boundaries against the landscape and bind them
# in a single sf object for plotly animation
intersected_maps <- lapply(all_maps, intersect_features)
sf_maps <- do.call(rbind, intersected_maps)
rownames(sf_maps) <- NULL


## }
## iplot(ooa, eur)

cast_world <- sf::st_cast(map, "MULTIPOLYGON")
cast_maps <- sf::st_cast(sf_maps, "MULTIPOLYGON")

## this works but has no coloring (plots one trace "trace 0" too)
plot_ly() %>%
  add_sf(data = cast_maps, fill = ~pop, frame = ~-time,
        text = ~pop, hoverinfo = "text") %>%
  animation_opts(transition = 0, redraw = FALSE)

# this works including coloring and everything but is not animated yet
cast_maps %>%
  plot_ly(split = ~pop, color = ~pop)



# adding a frame breaks shit
cast_maps %>%
  plot_ly(split = ~pop, color = ~pop, frame = ~time)






# this works but has no color
p <- ggplot() +
    p_map +
    theme_bw() +
    geom_sf(data = cast_maps, aes(frame = -time, text = pop)) +
  p_coord
p

plotly::ggplotly(p) %>%
  plotly::animation_opts(transition = 0, redraw = FALSE)




a <- filter(cast_maps, pop == "YAM_migr")
b <- filter(cast_maps, pop != "YAM_migr", !time %in% a$time)
a$time <- a$time + rnorm(nrow(a), sd = 100)
y <- rbind(a, b)

# this works but has no color
p <- ggplot() +
    p_map +
    theme_bw() +
    geom_sf(data = y, aes(frame = -time, text = pop, fill = pop), alpha = 0.5) +
    p_coord
p

plotly::ggplotly(p) %>%
  plotly::animation_opts(transition = 0, redraw = FALSE)




























accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- txhousing 
fig <- df %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area"))
fig <- fig %>% accumulate_by(~date)

