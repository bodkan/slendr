msg <- "Cannot schedule sampling"

test_that("sampling from a population which is not present is prevented (forward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), dir = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_error(sampling(model, time = 1, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 1000, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling from a population which is not present is prevented (backward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 1000, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 900, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), dir = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_error(sampling(model, time = 2000, list(p2, 3), strict = TRUE), msg) # pre-split
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
