test_that("circular ranges need two parameters", {
  map <- readRDS("map.rds")
  expect_error(
    population("pop", parent = "ancestor", time = 1, N = 1, map = map, center = 1),
    "Missing radius argument when defining a circular population range"
  )
  expect_error(
    population("pop", parent = "ancestor", time = 1, N = 1, map = map, radius = 1),
    "Missing center argument when defining a circular population range"
  )
})

test_that("polygon boundary can be specified in two ways", {
  map <- readRDS("map.rds")
  coords <- list(c(-18, 20), c(40, 20), c(30, 33), c(20, 32), c(10, 35), c(-8, 35))
  africa <- region("Africa", map, polygon = coords)
  afr1 <- population("AFR", parent = "ancestor", time = 1, N = 1, map = map, polygon = africa)
  afr2 <- population("AFR", parent = "ancestor", time = 1, N = 1, map = map, polygon = coords)
  expect_equal(afr1, afr2)
})

test_that("polygon ranges need at least three corners", {
  map <- readRDS("map.rds")
  coords <- list(c(-18, 20), c(40, 20))
  expect_error(
    region("Africa", map, polygon = coords),
    "Polygon range needs to have at least three corners"
  )
})

test_that("polygon ranges need at least three corners", {
  map <- readRDS("map.rds")
  coords <- list(c(-18, 20), c(40, 20), c(50, 50))
  expect_error(
    population("AFR", parent = "ancestor", time = 1, N = 1, map = map, center = 1, radius = 1, polygon = coords),
    "Either a circular range (center and radius) or the corners of a polygon need to be specified, but not both", fixed = TRUE
  )
})

test_that("union of a region with itself gives the same region", {
  map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "blank")
  coords <- list(c(-18, 20), c(40, 20), c(30, 33), c(20, 32), c(10, 35), c(-8, 35))
  africa <- region("Africa", map, polygon = coords)
  expect_true(all(africa == join(africa, africa, name = "Africa")))
})

test_that("intersection of a region with itself gives the same region", {
  map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "blank")
  coords <- list(c(-18, 20), c(40, 20), c(30, 33), c(20, 32), c(10, 35), c(-8, 35))
  africa <- region("Africa", map, polygon = coords)
  expect_true(all(africa == overlap(africa, africa, name = "Africa")))
})

test_that("subtraction of a region from itself gives an empty region", {
  map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "blank")
  coords <- list(c(-18, 20), c(40, 20), c(30, 33), c(20, 32), c(10, 35), c(-8, 35))
  africa <- region("Africa", map, polygon = coords)
  expect_error(
    subtract(africa, africa, name = ""),
    "No region left after subtraction"
  )
})

test_that("empty subtraction result gives an error", {
  map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "blank")
  coords <- list(c(-18, 20), c(40, 20), c(30, 33), c(20, 32), c(10, 35), c(-8, 35))
  africa <- region("Africa", map, polygon = coords)
  expect_error(
    subtract(africa, africa, name = ""),
    "No region left after subtraction"
  )
})

test_that("subtraction of a non-overlapping region does not change the result", {
  map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "blank")
  r1 <- region("r1", map, center = c(0, 0), radius = 1)
  r2 <- region("r2", map, center = c(10, 10), radius = 1)
  expect_true(all(subtract(r1, r2, name = "r1") == r1))
})

test_that("empty intersection gives an error", {
  map <- world(xrange = c(-15, 60), yrange = c(20, 65), landscape = "blank")
  r1 <- region("r1", map, center = c(0, 0), radius = 1)
  r2 <- region("r2", map, center = c(10, 10), radius = 1)
  expect_error(overlap(r1, r2, ""), "No region left after intersection")
})

test_that("custom landscapes can be specified", {
  r1 <- region(center = c(0, 0), radius = 10)
  r2 <- region(center = c(10, 10), radius = 10)
  r3 <- region(center = c(-10, -10), radius = 10)
  comb <- r1 %>% join(r2) %>% join(r3)
  map <- world(xrange = c(-20, 20), yrange = c(-20, 20), landscape = comb)
  # is the geometry component of the builtin map equal to the are of
  # the original regions?
  expect_true(sf::st_area(map$landscape) == sf::st_area(comb))
})

test_that("boundaries are either circles or polygons", {
  expect_error(region(), "Either a circular range or a polygon range must be specified")
})
