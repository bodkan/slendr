test_that("forward and backward time units are equivalent", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")

  # forward direction
  p1 <- population(name = "p1", parent = "ancestor", time = 1, N = 1, center = c(1, 1), radius = 1, map = map)
  p2 <- population(name = "p2", parent = p1,         time = 2, N = 1, center = c(1, 1), radius = 1)
  p3 <- population(name = "p3", parent = p1,         time = 3, N = 1, center = c(1, 1), radius = 1)
  p4 <- population(name = "p4", parent = p3,         time = 4, N = 1, center = c(1, 1), radius = 1)
  p5 <- population(name = "p5", parent = p2,         time = 5, N = 1, center = c(1, 1), radius = 1)

  geneflows <- geneflow(p1, p2, start = 2, end = 3, rate = 0.5, overlap = FALSE)

  forward <- compile(
    dir = file.path(tempdir(), "tmp-forward"),
    populations = list(p1, p2, p3, p4, p5), geneflows = geneflows,
    generation_time = 1, resolution = 1,
    overwrite = TRUE,
    competition_dist = 1, mate_dist = 1, offspring_dist = 1
  )

  # backward direction
  p1 <- population(name = "p1", parent = "ancestor", time = 5, N = 1, center = c(1, 1), radius = 1, map = map)
  p2 <- population(name = "p2", parent = p1,         time = 4, N = 1, center = c(1, 1), radius = 1)
  p3 <- population(name = "p3", parent = p1,         time = 3, N = 1, center = c(1, 1), radius = 1)
  p4 <- population(name = "p4", parent = p3,         time = 2, N = 1, center = c(1, 1), radius = 1)
  p5 <- population(name = "p5", parent = p2,         time = 1, N = 1, center = c(1, 1), radius = 1)

  geneflows <- geneflow(p1, p2, start = 4, end = 3, rate = 0.5, overlap = FALSE)

  backward <- compile(
    dir = file.path(tempdir(), "tmp-backward"),
    populations = list(p1, p2, p3, p4, p5), geneflows = geneflows,
    generation_time = 1, resolution = 1,
    overwrite = TRUE,
    competition_dist = 1, mate_dist = 1, offspring_dist = 1
  )

  expect_true(all.equal(forward$splits, backward$splits))
  expect_true(all.equal(geneflows$splits, geneflows$splits))
  expect_true(all.equal(forward$maps[, c("pop", "pop_id", "time", "time_gen")],
                        backward$maps[, c("pop", "pop_id", "time", "time_gen")]))

  components <- c("generation_time", "resolution", "world")
  expect_true(all(sapply(components, function(i) all.equal(forward[[i]], backward[[i]]))))
})
