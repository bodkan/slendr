# world <- readRDS("tests/testthat/world.rds")

test_that("read() restores a single-map model object", {
  world <- readRDS("world.rds")
  pop <- population("pop", parent = "ancestor", N = 10,
                    center = c(10, 40), radius = 100, world = world)

  model_dir <- file.path(tempdir(), "tmp-single-map-model-serialization")
  model1 <- compile(pop, model_dir = model_dir, resolution = 10, gen_time = 1, overwrite = TRUE)
  model2 <- read(model1$config$directory)

  # make sure that all components of the model list object before and after
  # serialization are equal
  components <- c("splits", "admixtures", "maps", "gen_time", "resolution", "world")
  expect_true(all(sapply(components, function(i) all(model1[[i]] == model2[[i]]))))
  expect_true(all(unlist(model1$config) == unlist(model2$config)))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(model1$populations[[i]] == model2$populations[[i]]))))
})


test_that("read() restores a complex model object", {
  world <- readRDS("world.rds")

  p1 <- population(name = "pop1", parent = "ancestor", N = 700, radius = 600, center = c(10, 25), world = world)
  p2 <- population(name = "pop2", parent = p1, time = 30000, N = 500, center = c(10, 25), radius = 300) %>%
    move(trajectory = list(c(25, 25), c(40, 30), c(40, 40), c(50, 50)), start = 29000, end = 25000, snapshots = 30)
  p3 <- population(name = "pop3", parent = p2, time = 20000, N = 2000, coords = list(c(-10, 50), c(10, 50), c(20, 53), c(40, 60), c(40, 70), c(-10, 65)))
  p4 <- population(name = "pop4", parent = p2, time = 15000, N = 2000, coords = list(c(-10, 35), c(20, 37), c(25, 40), c(30, 45), c(10, 50), c(-10, 45)))
  p5 <- population(name = "pop5", parent = p1, time = 10000, N = 300, center = c(10, 25), radius = 300) %>%
    move(trajectory = list(c(-5, 33), c(-5, 40)), start = 9000, end = 8000, snapshots = 20) %>%
    expand(by = 2000, start = 7000, end = 2000, snapshots = 10)

  admixtures <- list(
    admixture(from = p5, to = p4, rate = 0.2, start = 2000, end = 0),
    admixture(from = p5, to = p3, rate = 0.3, start = 2000, end = 0)
  )

  model_dir <- file.path(tempdir(), "tmp-complex-map-model-serialization")
  model1 <- compile(
    model_dir = model_dir,
    populations = list(p1, p2, p3, p4, p5),
    admixtures = admixtures,
    gen_time = 30,
    resolution = 10,
    overwrite = TRUE
  )
  model2 <- read(model1$config$directory)

  components <- c("splits", "admixtures", "maps", "gen_time", "resolution", "world")
  expect_true(all(sapply(components, function(i) all(model1[[i]] == model2[[i]]))))
  expect_true(all(unlist(model1$config) == unlist(model2$config)))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(model1$populations[[i]] == model2$populations[[i]]))))
})
