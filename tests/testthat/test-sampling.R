msg <- "Cannot schedule sampling"

test_that("sampling from a population which is not present is prevented (forward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), path = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_error(sampling(model, time = 15, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 1000, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling from a population which is not present is prevented (backward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 1000, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 900, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), path = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_error(sampling(model, time = 950, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 5, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("invalid sampling results in a warning", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile(populations = list(p1, p2), path = tempdir(), generation_time = 1, resolution = 1, sim_length = 1000, overwrite = TRUE, competition_dist = 1, mate_dist = 1, dispersal_dist = 1)
  expect_warning(sampling(model, times = 10000, list(p1, 1), list(p2, 1)), "No valid sampling events were retained")
  suppressWarnings({res <- sampling(model, times = 10000, list(p1, 1), list(p2, 1))})
  expect_null(res)
})

msg <- "A sampling event was scheduled outside of the simulation time window"

test_that("sampling before a simulation start (forward)", {
  p1 <- population(name = "p1", time = 10, N = 1, remove = 100)
  model <- compile(populations = p1, path = tempdir(), generation_time = 1, sim_length = 1000, overwrite = TRUE)
  expect_error(sampling(model, time = 5, list(p1, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 2000, list(p1, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling before a simulation start (backward)", {
  p1 <- population(name = "p1", time = 1000, N = 1, remove = 100)
  model <- compile(populations = p1, path = tempdir(), generation_time = 1, sim_length = 100, overwrite = TRUE)
  expect_error(sampling(model, time = 1005, list(p1, 3), strict = TRUE), msg) # pre-split
  expect_error(sampling(model, time = 10, list(p1, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling in the same generation of the split is prevented (forward)", {
  p1 <- population(name = "p1", time = 1, N = 1, remove = 50)
  model <- compile(populations = p1, path = tempdir(), generation_time = 25, sim_length = 100, overwrite = TRUE)
  expect_warning(sampling(model, time = 3, list(p1, 3)), "No valid sampling")
})

test_that("sampling in the same generation of the split is prevented (backward)", {
  p1 <- population(name = "p1", time = 100, N = 1, remove = 10)
  model <- compile(populations = p1, path = tempdir(), generation_time = 25, overwrite = TRUE)
  expect_warning(sampling(model, time = 97, list(p1, 3)), "No valid sampling")
})

# spatial sampling --------------------------------------------------------

test_that("only locations within world bounds are valid", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  valid <- list(c(100, 100), c(0, 0), c(30, 5))
  invalid <- list(c(1000, 100), c(0, 0), c(30, 5))
  expect_error(check_location_bounds(invalid, map), "locations fall outside")
  expect_silent(check_location_bounds(valid, map))
})

test_that("sampling is as close to the a single specified position as possible", {
  skip_if(!env_present("automatic_slendr_python_env")); setup_env(quiet = TRUE)

  n_samples <- 5
  times <- c(10, 100)
  locations <- list(c(50, 50))
  sim_length <- 100

  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", 1, 100, map = map, center = c(50, 50), radius = 50)
  model <- compile(pop, path = tempdir(), generation_time = 1,
                   competition_dist = 10, mate_dist = 10, dispersal_dist = 10,
                   sim_length = sim_length, resolution = 1, overwrite = TRUE)

  samples <- sampling(model, times = times, locations = locations, list(pop, n_samples))
  slim(model, sequence_length = 1, recombination_rate = 0, sampling = samples,
       method = "batch", save_locations = TRUE, verbose = FALSE)

  # load the locations of all individuals throughout the simulation, and filter
  # down to `n_sample` of the ones closest to the sampling location [50, 50] in
  # each time point (essentially replicating what we're doing on the SLiM
  # backend during the simulation run)
  locations <- file.path(model$path, "output_ind_locations.tsv.gz") %>%
    readr::read_tsv(show_col_types = FALSE) %>%
    dplyr::mutate(time = sim_length - gen + 1) %>%
    dplyr::filter(time %in% times) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(distance = sqrt((50 - x)^2 + (50 - y)^2)) %>%
    dplyr::arrange(time, distance) %>%
    dplyr::mutate(i = 1:dplyr::n()) %>%
    dplyr::filter(i %in% 1:n_samples) %>%
    dplyr::ungroup() %>%
    dplyr::select(ind, time, x, y, distance) %>%
    dplyr::arrange(time, distance) %>%
    as.data.frame()

  # load locations of individuals remembered in the tree sequence
  ts <- ts_load(model)
  individuals <- ts_data(ts, remembered = TRUE) %>%
    dplyr::select(-node_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      x = sf::st_coordinates(.)[, "X"],
      y = sf::st_coordinates(.)[, "Y"]
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::select(ind_id, time, x, y) %>%
    dplyr::mutate(distance = sqrt((50 - x)^2 + (50 - y)^2)) %>%
    dplyr::arrange(time, distance) %>%
    as.data.frame()

  expect_true(all.equal(individuals$x, locations$x, tolerance = 0.001))
  expect_true(all.equal(individuals$y, locations$y, tolerance = 0.001))
  expect_true(all.equal(individuals$distance, locations$distance, tolerance = 0.001))
})

test_that("sampling is as close to the multiple specified positions as possible", {
  skip_if(!env_present("automatic_slendr_python_env")); setup_env(quiet = TRUE)

  n_samples <- 5
  times <- c(10, 100)
  locations <- list(c(0, 0), c(100, 100))
  sim_length <- 100

  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", 1, 100, map = map, center = c(50, 50), radius = 50)
  model <- compile(pop, path = tempdir(), generation_time = 1,
                   competition_dist = 10, mate_dist = 10, dispersal_dist = 10,
                   sim_length = sim_length, resolution = 1, overwrite = TRUE)

  samples <- rbind(
    sampling(model, times = times[1], locations = locations[1], list(pop, n_samples)),
    sampling(model, times = times[2], locations = locations[2], list(pop, n_samples))
  )

  slim(model, sequence_length = 1, recombination_rate = 0, sampling = samples,
       method = "batch", save_locations = TRUE, verbose = FALSE)

  # load the locations of all individuals throughout the simulation, and filter
  # down to `n_sample` of the ones closest to the sampling location [50, 50] in
  # each time point (essentially replicating what we're doing on the SLiM
  # backend during the simulation run)
  all_locations <- file.path(model$path, "output_ind_locations.tsv.gz") %>%
    readr::read_tsv(show_col_types = FALSE) %>%
    dplyr::mutate(time = sim_length - gen + 1)

  first_loc <- all_locations %>%
    dplyr::filter(time == times[1]) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(distance = sqrt((locations[[1]][1] - x)^2 + (locations[[1]][2] - y)^2)) %>%
    dplyr::arrange(time, distance) %>%
    dplyr::mutate(i = 1:dplyr::n()) %>%
    dplyr::filter(i %in% 1:n_samples) %>%
    dplyr::ungroup() %>%
    dplyr::select(ind, time, x, y, distance) %>%
    dplyr::arrange(time, distance) %>%
    as.data.frame()

  second_loc <- all_locations %>%
    dplyr::filter(time == times[2]) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(distance = sqrt((locations[[2]][1] - x)^2 + (locations[[2]][2] - y)^2)) %>%
    dplyr::arrange(time, distance) %>%
    dplyr::mutate(i = 1:dplyr::n()) %>%
    dplyr::filter(i %in% 1:n_samples) %>%
    dplyr::ungroup() %>%
    dplyr::select(ind, time, x, y, distance) %>%
    dplyr::arrange(time, distance) %>%
    as.data.frame()

  locs <- rbind(first_loc, second_loc)

  # load locations of individuals remembered in the tree sequence
  ts <- ts_load(model)
  all_individuals <- ts_data(ts, remembered = TRUE) %>%
    dplyr::select(-node_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      x = sf::st_coordinates(.)[, "X"],
      y = sf::st_coordinates(.)[, "Y"]
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::select(ind_id, time, x, y)

  first_individuals <- all_individuals %>%
    dplyr::filter(time == times[1]) %>%
    dplyr::mutate(distance = sqrt((locations[[1]][1] - x)^2 + (locations[[1]][2] - y)^2)) %>%
    dplyr::arrange(time, distance) %>%
    as.data.frame()

  second_individuals <- all_individuals %>%
    dplyr::filter(time == times[2]) %>%
    dplyr::mutate(distance = sqrt((locations[[2]][1] - x)^2 + (locations[[2]][2] - y)^2)) %>%
    dplyr::arrange(time, distance) %>%
    as.data.frame()

  individuals <- rbind(first_individuals, second_individuals)

  expect_true(all.equal(individuals$x, locs$x, tolerance = 0.001))
  expect_true(all.equal(individuals$y, locs$y, tolerance = 0.001))
  expect_true(all.equal(individuals$distance, locs$distance, tolerance = 0.001))
})
