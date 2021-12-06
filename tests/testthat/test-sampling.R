msg <- "Cannot schedule sampling"

test_that("sampling from a population which is not present is prevented (forward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), dir = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_error(sampling(model, time = 15, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 1000, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling from a population which is not present is prevented (backward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 1000, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 900, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), dir = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_error(sampling(model, time = 950, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 5, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("invalid sampling results in a warning", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), dir = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_warning(sampling(model, times = 10000, list(p1, 1), list(p2, 1)), "No valid sampling events were retained")
  suppressWarnings({res <- sampling(model, times = 10000, list(p1, 1), list(p2, 1))})
  expect_null(res)
})

msg <- "A sampling event was scheduled outside of the simulation time window"

test_that("sampling before a simulation start (forward)", {
  p1 <- population(name = "p1", time = 10, N = 1, remove = 100)
  model <- compile(populations = p1, dir = tempdir(), generation_time = 1, sim_length = 1000, overwrite = TRUE)
  expect_error(sampling(model, time = 5, list(p1, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 2000, list(p1, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling before a simulation start (backward)", {
  p1 <- population(name = "p1", time = 1000, N = 1, remove = 100)
  model <- compile(populations = p1, dir = tempdir(), generation_time = 1, sim_length = 100, overwrite = TRUE)
  expect_error(sampling(model, time = 1005, list(p1, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 10, list(p1, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling in the same generation of the split is prevented (forward)", {
  p1 <- population(name = "p1", time = 1, N = 1, remove = 50)
  model <- compile(populations = p1, dir = tempdir(), generation_time = 25, sim_length = 100, overwrite = TRUE)
  expect_warning(sampling(model, time = 3, list(p1, 3)), "No valid sampling")
})

test_that("sampling in the same generation of the split is prevented (backward)", {
  p1 <- population(name = "p1", time = 100, N = 1, remove = 10)
  model <- compile(populations = p1, dir = tempdir(), generation_time = 25, overwrite = TRUE)
  expect_warning(sampling(model, time = 97, list(p1, 3)), "No valid sampling")
})
