test_that("forward and backward time units are equivalent", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")

  p1 <- population(name = "p1", parent = "ancestor", time = 5, N = 1, center = c(1, 1), radius = 1, map = map)
  p2 <- population(name = "p2", parent = p1,         time = 4, N = 1, center = c(1, 1), radius = 1)
  p3 <- population(name = "p3", parent = p1,         time = 3, N = 1, center = c(1, 1), radius = 1)
  p4 <- population(name = "p4", parent = p3,         time = 2, N = 1, center = c(1, 1), radius = 1)
  p5 <- population(name = "p5", parent = p2,         time = 1, N = 1, center = c(1, 1), radius = 1)

  model1 <- compile(
    dir = file.path(tempdir(), "tmp-complex-map-model-serialization"),
    populations = list(p1, p2, p3, p4, p5),
    generation_time = 1, resolution = 1,
    overwrite = TRUE,
    competition_dist = 1, mate_dist = 1, offspring_dist = 1
  )

  components <- c("splits", "geneflows", "maps", "generation_time", "resolution", "world")
  expect_true(all(sapply(components, function(i) all.equal(model1[[i]], model2[[i]]))))
  expect_true(all(unlist(model1$config) == unlist(model2$config)))
  expect_true(all(sapply(seq_along(model1$populations), function(i) all(model1$populations[[i]] == model2$populations[[i]]))))
})