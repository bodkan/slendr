test_that("'competition_dist' must be specified in compile() if missing", {
  map <- readRDS("map.rds")
  p <- population(mate_dist = 10, dispersal_dist = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile(populations = list(p), generation_time = 30, resolution = 11e3, path = tempfile(), overwrite = TRUE, direction = "backward"),
               "Parameter 'competition_dist' missing", fixed = TRUE)
  expect_silent(compile(competition_dist = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, direction = "backward"))
})

test_that("'mate_dist' must be specified in compile() if missing", {
  map <- readRDS("map.rds")
  p <- population(competition_dist = 10, dispersal_dist = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, direction = "backward"),
               "Parameter 'mate_dist' missing", fixed = TRUE)
  expect_silent(compile(mate_dist = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, direction = "backward"))
})

test_that("'dispersal_dist' must be specified in compile() if missing", {
  map <- readRDS("map.rds")
  p <- population(competition_dist = 10, mate_dist = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, direction = "backward"),
               "Parameter 'dispersal_dist' missing", fixed = TRUE)
  expect_silent(compile(dispersal_dist = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, direction = "backward"))
})

test_that("'competition_dist', 'mate_dist', and 'dispersal_dist' do not have to be specified in compile() if already present", {
  map <- readRDS("map.rds")
  p <- population(competition_dist = 10, mate_dist = 10, dispersal_dist = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_silent(compile(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, direction = "backward"))
})

test_that("presence of all parents is enforced", {
  p1 <- population(name = "pop1", N = 700, time = 40000)
  p2 <- population(name = "pop2", parent = p1, N = 700, time = 4000)
  expect_error(compile(populations = p2, path = file.path(tempdir(), "missing-parent"), generation_time = 30),
               "The following parent populations are missing: pop1")
})

test_that("invalid blank maps are prevented", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", time = 1, N = 100, map = map, center = c(50, 50), radius = 0.5)

  expect_error(compile(pop, tempdir(), generation_time = 1, competition_dist = 1,
                       mate_dist = 50, dispersal_dist = 1, sim_length = 300,
                       resolution = 1, overwrite = TRUE),
               "No occupiable pixel on a rasterized map")
})

test_that("`dir is deprecated in favor of `path`", {
  p <- population(name = "pop", N = 700, time = 100) %>% resize(N = 100, time = 50, how = "step")
  expect_warning(model <- compile(p, dir = file.path(tempdir(), "dir-rerouted-to-path"), generation_time = 30, overwrite = TRUE),
                 "The `dir =` argument of the `compile\\(\\)` function is now deprecated.")
  expect_true(grepl("dir-rerouted-to-path$", model$path))
})
