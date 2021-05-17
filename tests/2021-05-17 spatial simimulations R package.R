
# Three big new features:
#  1. abstract landscapes
#  2. set operations on geographic regions
#  3. shiny exploration app




# 1.  abstract landscapes -------------------------------------------------




# real maps ---------------------------------------------------------------

map_real <- world(
  xrange = c(-15, 60),
  yrange = c(20, 65),
  landscape = "naturalearth",
  crs = "EPSG:3035",
  ne_dir = "~/Google/postdoc/data/ne_data/"
)

map_real

plot(map_real)

# populations on this real map...

afr <- population(
  "AFR", parent = "ancestor", time = 60000, N = 2000, map = map_real,
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

plot(afr)
plot(ooa)




# abstract maps (blank) ---------------------------------------------------

map_blank <- world(
  xrange = c(0, 100),
  yrange = c(0, 100),
  landscape = "blank"
)

map_blank

plot(map_blank)

p1 <- population("p1", N = 100, time = 10000, parent = "ancestor",
                 map = map_blank, center = c(20, 20), radius = 10)

p2 <- population("p2", N = 1000, time = 10000, parent = p1,
                 map = map_blank, center = c(80, 80), radius = 15)

p3 <- population("p3", N = 100, time = 10000, parent = p2,
                 map = map_blank, center = c(80, 20), radius = 8) %>%
  move(trajectory = list(c(80, 80), c(20, 60)),
       start = 8000, end = 5000, snapshots = 20)

plot(p1, p2, p3, pop_facets = F)

model_blank <- compile(
  populations = list(p1, p2, p3),
  dir = "/tmp/model-blank",
  generation_time = 1, resolution = 2,
  overwrite = T
)

explore(model_blank)






# abstract maps (user-defined landscape) ----------------------------------

land <- region("islandX", polygon  = list(c(-10, 30), c(50, 30),
                                          c(40, 50), c(0, 40)))
land

map_custom <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = land)
map_custom

plot(map_custom)

p1 <- population("p1", N = 100, time = 10000, parent = "ancestor",
                 map = map_custom, center = c(0, 30), radius = 5) %>%
  expand(by = 15, start = 8000, end = 6000, snapshots = 10)

p2 <- population("p2", N = 1000, time = 10000, parent = p1,
                 map = map_custom, center = c(30, 45), radius = 8)

plot(p1, p2, pop_facets = F)


model_custom <- compile(
  populations = list(p1, p2),
  dir = "/tmp/model-custom",
  generation_time = 1, resolution = 2,
  overwrite = T
)

explore(model_custom)






# 2.  set operations ------------------------------------------------------


# three "geographic" regions ...
r1 <- region("r1", center = c(-10, -10), radius = 10)
r2 <- region("r2", polygon = list(c(-10, 0), c(-5, -10), c(10, 0),
                                  c(5, 10), c(-10, 10)))
r3 <- region("r3", center = c(10, 10), radius = 10)

plot(r1, r2, r3)

# three new set operation functions in our package
overlap(r1, r2) %>% plot
join(r1, r2) %>% plot
subtract(r3, r2) %>% plot

land <- join(overlap(r1, r2), subtract(r3, r2))
land

map_custom2 <- world(
  xrange = c(-15, 30),
  yrange = c(-15, 30),
  landscape = land
)

map_custom2

plot(map_custom2)

land$geometry[[1]]








# 3. shiny app ------------------------------------------------------------

# this originated as a debugging feature for myself, but it
# has only a limited use for a general user (the model is dynamic,
# but this doesn't capture any of this)
plot(afr, ooa, ana, eur)

# it's even worse considering that there is admixture
graph(model)

# interactive shiny app to the rescue
explore(model)
