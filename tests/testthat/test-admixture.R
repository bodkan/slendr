test_that("non-overlapping admixture leads to error", {
  world <- readRDS("world.rds")
  pop1 <- population("pop1", parent = "ancestor", N = 100,
                      center = c(0, 10), radius = 1, world = world, intersect = FALSE)
  pop2 <- population("pop2", parent = "ancestor", N = 100,
                     center = c(0, -10), radius = 1, world = world, intersect = FALSE)
  expect_error(admixture(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1))
})

test_that("non-overlapping admixture passes if the check is explicitly turned off", {
  world <- readRDS("world.rds")
  pop1 <- population("pop1", parent = "ancestor", N = 100,
                     center = c(0, 10), radius = 1, world = world, intersect = FALSE)
  pop2 <- population("pop2", parent = "ancestor", N = 100,
                     center = c(0, -10), radius = 1, world = world, intersect = FALSE)
  expect_s3_class(
    admixture(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1, overlap = FALSE),
    "data.frame"
  )
})
