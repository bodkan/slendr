map <- world(xrange = c(0, 1e6), yrange = c(0, 1e6), landscape = "blank")

test_that("population size is correctly decreased upon reducing circular boundary", {
  r1 <- 300e3
  r2 <- 100e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map, center = c(500, 500),
                    radius = r1) |>
    boundary(time = 500, center = c(500, 500), radius = r2, lock = TRUE)

  ind_df <- run_sim(pop, "forward", sim_length = 550, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- pi * r1^2
  area2 <- pi * r2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 500, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 500, "N"] == N2))
})

test_that("population size is correctly increased upon exanding circular baoundary", {
  r1 <- 100e3
  r2 <- 300e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map, center = c(500, 500),
                    radius = r1) |>
    boundary(time = 500, center = c(500, 500), radius = r2, lock = TRUE)

  ind_df <- run_sim(pop, "forward", sim_length = 550, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- pi * r1^2
  area2 <- pi * r2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 500, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 500, "N"] == N2))
})

test_that("population size is correctly decreased upon reducing square boundary", {
  a1 <- 300e3
  a2 <- 100e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map,
                    polygon = list(c(0, 0), c(a1, 0), c(a1, a1), c(0, a1))) |>
    boundary(time = 500, lock = TRUE, polygon = list(c(0, 0), c(a2, 0), c(a2, a2), c(0, a2)))

  ind_df <- run_sim(pop, "forward", sim_length = 550, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- a1^2
  area2 <- a2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 500, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 500, "N"] == N2))
})


test_that("population size is correctly decreased upon expanding square boundary", {
  a1 <- 100e3
  a2 <- 300e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map,
                    polygon = list(c(0, 0), c(a1, 0), c(a1, a1), c(0, a1))) |>
    boundary(time = 500, lock = TRUE, polygon = list(c(0, 0), c(a2, 0), c(a2, a2), c(0, a2)))

  ind_df <- run_sim(pop, "forward", sim_length = 550, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- a1^2
  area2 <- a2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 500, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 500, "N"] == N2))
})
