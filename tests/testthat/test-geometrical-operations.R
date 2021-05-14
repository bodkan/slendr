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
