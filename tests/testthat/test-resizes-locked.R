skip_if(!env_present("automatic_slendr_python_env")); setup_env(quiet = TRUE)

map <- world(xrange = c(0, 1e6), yrange = c(0, 1e6), landscape = "blank")

test_that("population size is correctly decreased upon reducing circular boundary", {
  r1 <- 300e3
  r2 <- 100e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map, center = c(500, 500),
                    radius = r1) %>%
    boundary(time = 50, center = c(500, 500), radius = r2, lock = TRUE)

  ind_df <- run_sim(pop, "forward", sim_length = 55, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- pi * r1^2
  area2 <- pi * r2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 50, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 50, "N"] == N2))
})

test_that("population size is correctly increased upon exanding circular baoundary", {
  r1 <- 100e3
  r2 <- 300e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map, center = c(500, 500),
                    radius = r1) %>%
    boundary(time = 50, center = c(500, 500), radius = r2, lock = TRUE)

  ind_df <- run_sim(pop, "forward", sim_length = 55, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- pi * r1^2
  area2 <- pi * r2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 50, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 50, "N"] == N2))
})

test_that("population size is correctly decreased upon reducing square boundary", {
  a1 <- 300e3
  a2 <- 100e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map,
                    polygon = list(c(0, 0), c(a1, 0), c(a1, a1), c(0, a1))) %>%
    boundary(time = 50, lock = TRUE, polygon = list(c(0, 0), c(a2, 0), c(a2, a2), c(0, a2)))

  ind_df <- run_sim(pop, "forward", sim_length = 55, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- a1^2
  area2 <- a2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 50, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 50, "N"] == N2))
})


test_that("population size is correctly decreased upon expanding square boundary", {
  a1 <- 100e3
  a2 <- 300e3
  N1 <- 100

  pop <- population("pop", time = 1, N = N1, map = map,
                    polygon = list(c(0, 0), c(a1, 0), c(a1, a1), c(0, a1))) %>%
    boundary(time = 50, lock = TRUE, polygon = list(c(0, 0), c(a2, 0), c(a2, a2), c(0, a2)))

  ind_df <- run_sim(pop, "forward", sim_length = 55, verbose = FALSE)

  # make sure that the new population size corresponds to the decrease in the
  # circular area of the population boundary
  area1 <- a1^2
  area2 <- a2^2

  N2 <- round(N1 * area2 / area1)

  expect_true(all(ind_df[ind_df$time < 50, "N"] == N1))
  expect_true(all(ind_df[ind_df$time >= 50, "N"] == N2))
})

test_that("population size is correctly increased during range expansion", {
  r_start <- 100e3
  by <- 300e3
  N_start <- 100
  snapshots <- 10

  pop <- population("pop", time = 1, N = N_start, map = map, center = c(500e3, 500e3),
                    radius = r_start) %>%
    expand(by = by, start = 50, end = 100, lock = TRUE, snapshots = snapshots)

  ind_df <- run_sim(pop, "forward", sim_length = 150, verbose = FALSE)

  area_start <- pi * r_start^2
  N_observed <- unique(ind_df$N)

  # compute predicted values of N as the circular range expands
  r_values <- seq(r_start, r_start + by, length = snapshots + 1)
  N_expected <- round(N_start * pi * r_values^2 / area_start)

  expect_true(all(N_observed == N_expected))
})

test_that("population size is correctly increased during range contraction", {
  r_start <- 400e3
  by <- 300e3
  N_start <- 100
  snapshots <- 10

  pop <- population("pop", time = 1, N = N_start, map = map, center = c(500e3, 500e3),
                    radius = r_start) %>%
    shrink(by = by, start = 50, end = 100, lock = TRUE, snapshots = snapshots)

  ind_df <- run_sim(pop, "forward", sim_length = 150, verbose = FALSE)

  area_start <- pi * r_start^2
  N_observed <- unique(ind_df$N)

  # compute predicted values of N as the circular range expands
  r_values <- seq(r_start, r_start - by, length = snapshots + 1)
  N_expected <- round(N_start * pi * r_values^2 / area_start)

  expect_true(all(N_observed == N_expected))
})
