# spatial models ----------------------------------------------------------

test_that("read_model() restores a single-map model object", {
  map <- readRDS("map.rds")
  pop <- population("pop", N = 10, time = 100,
                    center = c(10, 40), radius = 100000, map = map)

  model_dir <- file.path(tempdir(), "tmp-single-map-model-serialization")
  model1 <- compile_model(pop, path = model_dir, resolution = 10000, generation_time = 1, overwrite = TRUE, force = TRUE,
                    competition = 100e3, mating = 100e3, dispersal = 10e3, direction = "backward",
                    description = "This is a model description", time_units = "These are the model time units")
  model2 <- read_model(model1$path)

  # make sure that all components of the model list object before and after
  # serialization are equal
  components <- c("checksums", "splits", "resizes", "geneflow", "maps", "direction", "length", "generation_time", "resolution", "world", "description", "time_units")
  expect_true(all(sapply(components, function(i) all.equal(model1[[i]], model2[[i]]))))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(model1$populations[[i]] == model2$populations[[i]]))))
  expect_equal(names(model1$populations), names(model1$populations))
})

test_that("read_model() restores a complex model object", {
  map <- readRDS("map.rds")

  p1 <- population(name = "pop1", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  p2 <- population(name = "pop2", parent = p1, time = 30000, N = 500, center = c(10, 25), radius = 300000) %>%
    move(trajectory = list(c(25, 25), c(40, 30), c(40, 40), c(50, 50)), start = 29000, end = 25000, snapshots = 30)
  p3 <- population(name = "pop3", parent = p2, time = 20000, N = 2000, polygon = list(c(-10, 50), c(10, 50), c(20, 53), c(40, 60), c(40, 70), c(-10, 65)))
  p4 <- population(name = "pop4", parent = p2, time = 15000, N = 2000, polygon = list(c(-10, 35), c(20, 37), c(25, 40), c(30, 45), c(10, 50), c(-10, 45)))
  p5 <- population(name = "pop5", parent = p1, time = 10000, N = 300, center = c(10, 25), radius = 300000) %>%
    move(trajectory = list(c(-5, 33), c(-5, 40)), start = 9000, end = 8000, snapshots = 20) %>%
    expand_range(by = 2000000, start = 7000, end = 2000, snapshots = 10)

  geneflow <- list(
    gene_flow(from = p5, to = p4, rate = 0.2, start = 2000, end = 0),
    gene_flow(from = p5, to = p3, rate = 0.3, start = 2000, end = 0)
  )

  model_dir <- file.path(tempdir(), "tmp-complex-map-model-serialization")
  model1 <- compile_model(
    path = model_dir,
    populations = list(p1, p2, p3, p4, p5),
    gene_flow = geneflow,
    generation_time = 30,
    resolution = 10000,
    overwrite = TRUE, force = TRUE,
    competition = 100e3, mating = 100e3, dispersal = 10e3
  )
  model2 <- read_model(model1$path)

  components <- c("checksums", "splits", "resizes", "geneflow", "maps", "length", "orig_length", "direction", "generation_time", "resolution", "world", "description", "time_units")
  expect_true(all(sapply(components, function(i) all.equal(model1[[i]], model2[[i]]))))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(model1$populations[[i]] == model2$populations[[i]]))))
  expect_equal(names(model1$populations), names(model1$populations))
})

test_that("non-unique population names lead to error", {
  map <- readRDS("map.rds")

  p1 <- population(name = "pop1", N = 700, time = 1, radius = 600000, center = c(10, 25), map = map)
  p2 <- population(name = "pop2", N = 700, time = 1, radius = 600000, center = c(10, 25), map = map)
  p3 <- population(name = "pop2", N = 700, time = 1, radius = 600000, center = c(10, 25), map = map)
  model_dir <- file.path(tempdir(), "tmp-name-uniqueness")
  expect_error(
    compile_model(path = model_dir, populations = list(p1, p2, p3), generation_time = 30, resolution = 10000, overwrite = TRUE, force = TRUE, simulation_length = 10, competition = 100e3, mating = 100e3, dispersal = 10e3),
    "All populations must have unique names"
  )

  p1 <- population(name = "pop1", N = 700, time = 1, radius = 600000, center = c(10, 25), map = map)
  p2 <- population(name = "pop2", N = 700, time = 1, radius = 600000, center = c(10, 25), map = map)
  p3 <- population(name = "pop3", N = 700, time = 1, radius = 600000, center = c(10, 25), map = map)
  model_dir <- file.path(tempdir(), "tmp-name-uniqueness")
  skip_if(Sys.info()[["sysname"]] == "Windows") # new meaningless CRAN warnings
  expect_silent(compile_model(path = model_dir, populations = list(p1, p2, p3), generation_time = 30, resolution = 10000, overwrite = TRUE, force = TRUE, simulation_length = 10, competition = 100e3, mating = 100e3, dispersal = 10e3))
})

# non-spatial models ------------------------------------------------------

test_that("read_model() restores a single-map model object (nonspatial)", {
  pop <- population("pop", N = 10, time = 100)

  model_dir <- file.path(tempdir(), "tmp-single-map-model-serialization")
  model1 <- compile_model(pop, path = model_dir, generation_time = 1, overwrite = TRUE, force = TRUE, direction = "backward")
  model2 <- read_model(model1$path)

  # make sure that all components of the model list object before and after
  # serialization are equal
  components <- c("checksums", "splits", "resizes", "geneflow", "maps", "direction", "length", "generation_time", "resolution", "world", "description", "time_units")
  expect_true(all(sapply(components, function(i) all.equal(model1[[i]], model2[[i]]))))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(unlist(model1$populations[[i]]) == unlist(model2$populations[[i]])))))
})

test_that("read_model() restores a complex model object (nonspatial)", {
  p1 <- population(name = "pop1", N = 700, time = 40000)
  p2 <- population(name = "pop2", parent = p1, time = 30000, N = 500)
  p3 <- population(name = "pop3", parent = p2, time = 20000, N = 2000)
  p4 <- population(name = "pop4", parent = p2, time = 15000, N = 2000)
  p5 <- population(name = "pop5", parent = p1, time = 10000, N = 300)

  geneflow <- list(
    gene_flow(from = p5, to = p4, rate = 0.2, start = 2000, end = 0),
    gene_flow(from = p5, to = p3, rate = 0.3, start = 2000, end = 0)
  )

  model_dir <- file.path(tempdir(), "tmp-complex-map-model-serialization-nonspatial")
  model1 <- compile_model(
    path = model_dir,
    populations = list(p1, p2, p3, p4, p5),
    gene_flow = geneflow,
    generation_time = 30,
    overwrite = TRUE, force = TRUE,
    description = "This is a description",
    time_units = "These are time units"
  )
  model2 <- read_model(model1$path)

  components <- c("checksums", "splits", "resizes", "geneflow", "maps", "length", "orig_length", "direction", "generation_time", "resolution", "world", "description", "time_units")
  expect_true(all(sapply(components, function(i) all.equal(model1[[i]], model2[[i]]))))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(unlist(model1$populations[[i]]) == unlist(model2$populations[[i]])))))
  expect_equal(names(model1$populations), names(model1$populations))
})

test_that("non-unique population names lead to error (nonspatial)", {
  p1 <- population(name = "pop1", N = 700, time = 1)
  p2 <- population(name = "pop2", N = 700, time = 1)
  p3 <- population(name = "pop2", N = 700, time = 1)
  model_dir <- file.path(tempdir(), "tmp-name-uniqueness-nonspatial")
  expect_error(
    compile_model(path = model_dir, populations = list(p1, p2, p3), generation_time = 30, resolution = 10000, overwrite = TRUE, force = TRUE, simulation_length = 10),
    "All populations must have unique names"
  )

  p1 <- population(name = "pop1", N = 700, time = 1)
  p2 <- population(name = "pop2", N = 700, time = 1)
  p3 <- population(name = "pop3", N = 700, time = 1)
  model_dir <- file.path(tempdir(), "tmp-name-uniqueness-nonspatial")
  expect_silent(compile_model(path = model_dir, populations = list(p1, p2, p3), generation_time = 30, resolution = 10000, overwrite = TRUE, force = TRUE, simulation_length = 10))
})

test_that("checksums are enforced", {
  pop <- population("pop", N = 10, time = 100)

  model_dir <- file.path(tempdir(), "tmp-checksums")
  model1 <- compile_model(pop, path = model_dir, generation_time = 1, overwrite = TRUE, force = TRUE, direction = "backward")
  model2 <- read_model(model_dir)
  expect_warning(verify_checksums(file.path(model_dir, model1$checksums$file[1]),
                 paste0(model1$checksums$hash[1], "asdf")), "Checksum of .* does not match")
  expect_equal(model1$checksums$hash, model2$checksums$hash)
})
