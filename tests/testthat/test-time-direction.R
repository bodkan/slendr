test_that("forward and backward time units are equivalent", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")

  # forward direction
  f1 <- population(name = "f1", parent = "ancestor", time = 1, N = 1, center = c(1, 1), radius = 1, map = map)
  f2 <- population(name = "f2", parent = f1,         time = 2, N = 1, center = c(1, 1), radius = 1)
  f3 <- population(name = "f3", parent = f1,         time = 3, N = 1, center = c(1, 1), radius = 1)
  f4 <- population(name = "f4", parent = f3,         time = 4, N = 1, center = c(1, 1), radius = 1)
  f5 <- population(name = "f5", parent = f2,         time = 5, N = 1, center = c(1, 1), radius = 1)

  geneflows <- geneflow(f1, f2, start = 2, end = 3, rate = 0.5, overlap = FALSE)

  forward <- compile(
    dir = file.path(tempdir(), "tmp-forward"),
    populations = list(f1, f2, f3, f4, f5), geneflows = geneflows,
    generation_time = 1, resolution = 1,
    overwrite = TRUE,
    competition_dist = 1, mate_dist = 1, offspring_dist = 1
  )

  # backward direction
  b1 <- population(name = "b1", parent = "ancestor", time = 5, N = 1, center = c(1, 1), radius = 1, map = map)
  b2 <- population(name = "b2", parent = b1,         time = 4, N = 1, center = c(1, 1), radius = 1)
  b3 <- population(name = "b3", parent = b1,         time = 3, N = 1, center = c(1, 1), radius = 1)
  b4 <- population(name = "b4", parent = b3,         time = 2, N = 1, center = c(1, 1), radius = 1)
  b5 <- population(name = "b5", parent = b2,         time = 1, N = 1, center = c(1, 1), radius = 1)

  geneflows <- geneflow(b1, b2, start = 3, end = 2, rate = 0.5, overlap = FALSE)

  backward <- compile(
    dir = file.path(tempdir(), "tmp-backward"),
    populations = list(b1, b2, b3, b4, b5), geneflows = geneflows,
    generation_time = 1, resolution = 1,
    overwrite = TRUE,
    competition_dist = 1, mate_dist = 1, offspring_dist = 1
  )

  components <- c("splits", "geneflows", "maps", "generation_time", "resolution", "world")
  expect_true(all(sapply(components, function(i) all.equal(model1[[i]], model2[[i]]))))
  expect_true(all(unlist(model1$config) == unlist(model2$config)))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(model1$populations[[i]] == model2$populations[[i]]))))
})