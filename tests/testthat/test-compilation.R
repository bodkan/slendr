test_that("'competition' must be specified in compile_model() if missing", {
  map <- readRDS("map.rds")
  p <- population(mating = 10, dispersal = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile_model(populations = list(p), generation_time = 30, resolution = 11e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"),
               "Parameter 'competition' missing", fixed = TRUE)
  expect_silent(compile_model(competition = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("'mating' must be specified in compile_model() if missing", {
  map <- readRDS("map.rds")
  p <- population(competition = 10, dispersal = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile_model(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"),
               "Parameter 'mating' missing", fixed = TRUE)
  expect_silent(compile_model(mating = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("'dispersal' must be specified in compile_model() if missing", {
  map <- readRDS("map.rds")
  p <- population(competition = 10, mating = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile_model(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"),
               "Parameter 'dispersal' missing", fixed = TRUE)
  expect_silent(compile_model(dispersal = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("'competition', 'mating', and 'dispersal' do not have to be specified in compile_model() if already present", {
  map <- readRDS("map.rds")
  p <- population(competition = 10, mating = 10, dispersal = 10, name = "pop1", parent = "ancestor", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_silent(compile_model(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("presence of all parents is enforced", {
  p1 <- population(name = "pop1", N = 700, time = 40000)
  p2 <- population(name = "pop2", parent = p1, N = 700, time = 4000)
  expect_error(compile_model(populations = p2, path = file.path(tempdir(), "missing-parent"), generation_time = 30),
               "The following parent populations are missing: pop1")
})

test_that("invalid blank maps are prevented", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", time = 1, N = 100, map = map, center = c(50, 50), radius = 0.5)

  expect_error(compile_model(pop, generation_time = 1, competition = 1,
                       mating = 50, dispersal = 1, simulation_length = 300,
                       resolution = 1),
               "No occupiable pixel on a rasterized map")
})

test_that("deletion in non-interactive modem must be forced", {
  p <- population(name = "pop", N = 700, time = 100) %>% resize(N = 100, time = 50, how = "step")
  directory <- file.path(tempdir(), "dir-forced")
  dir.create(directory)
  expect_error(model <- compile_model(p, path = directory, generation_time = 30, overwrite = TRUE),
               "Compilation aborted")
  model <- compile_model(p, path = directory, generation_time = 30, overwrite = TRUE, force = TRUE)
  expect_true(grepl("dir-forced$", model$path))
})

test_that("sequence length can only be an integer number (SLiM)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Sequence length must be a non-negative integer number"
  expect_error(slim(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_error(slim(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_silent(slim(model, sequence_length = 1e6, recombination_rate = 0))
})

test_that("sequence length can only be an integer number (msprime)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Sequence length must be a non-negative integer number"
  expect_error(msprime(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_error(msprime(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_silent(msprime(model, sequence_length = 1e6, recombination_rate = 0))
})

test_that("recombination rate can only be an integer number (SLiM)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Recombination rate must be a numeric value"
  expect_error(slim(model, sequence_length = 100, recombination_rate = "asdf"), error_msg)
  expect_error(slim(model, sequence_length = 100, recombination_rate = -1), error_msg)
  expect_silent(slim(model, sequence_length = 100, recombination_rate = 1e-8))
})

test_that("recombination rate can only be an integer number (msprime)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Recombination rate must be a numeric value"
  expect_error(msprime(model, sequence_length = 100, recombination_rate = "asdf"), error_msg)
  expect_error(msprime(model, sequence_length = 100, recombination_rate = -1), error_msg)
  expect_silent(msprime(model, sequence_length = 100, recombination_rate = 1e-8))
})
