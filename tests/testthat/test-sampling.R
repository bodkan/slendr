msg <- "Cannot schedule sampling"

test_that("sampling from a population which is not present is prevented (forward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile_model(populations = list(p1, p2), path = tempdir(), generation_time = 1, resolution = 1, simulation_length = 1000, overwrite = TRUE, force = TRUE, competition = 1, mating = 1, dispersal = 1)
  expect_error(schedule_sampling(model, time = 15, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(schedule_sampling(model, time = 1000, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling from a population which is not present is prevented (backward)", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 1000, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 900, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile_model(populations = list(p1, p2), path = tempdir(), generation_time = 1, resolution = 1, simulation_length = 1000, overwrite = TRUE, force = TRUE, competition = 1, mating = 1, dispersal = 1)
  expect_error(schedule_sampling(model, time = 950, list(p2, 3), strict = TRUE), msg) # pre-split
  expect_error(schedule_sampling(model, time = 5, list(p2, 3), strict = TRUE), msg) # post-removal
})

test_that("invalid sampling results in a warning", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 1, center = c(1, 1), radius = 10, map = map, remove = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 1, center = c(1, 1), radius = 10, remove = 100)
  model <- compile_model(populations = list(p1, p2), path = tempdir(), generation_time = 1, resolution = 1, simulation_length = 1000, overwrite = TRUE, force = TRUE, competition = 1, mating = 1, dispersal = 1)
  expect_warning(schedule_sampling(model, times = 10000, list(p1, 1), list(p2, 1)), "No valid sampling events were retained")
  suppressWarnings({res <- schedule_sampling(model, times = 10000, list(p1, 1), list(p2, 1))})
  expect_null(res)
})

msg <- "A sampling event was scheduled outside of the simulation time window"

test_that("sampling before a simulation start (forward)", {
  p1 <- population(name = "p1", time = 10, N = 1, remove = 100)
  model <- compile_model(populations = p1, path = tempdir(), generation_time = 1, simulation_length = 1000, overwrite = TRUE, force = TRUE)
  expect_error(schedule_sampling(model, time = 5, list(p1, 3), strict = TRUE), msg) # pre-split
  expect_error(schedule_sampling(model, time = 2000, list(p1, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling before a simulation start (backward)", {
  p1 <- population(name = "p1", time = 1000, N = 1, remove = 100)
  model <- compile_model(populations = p1, path = tempdir(), generation_time = 1, simulation_length = 100, overwrite = TRUE, force = TRUE)
  expect_error(schedule_sampling(model, time = 1005, list(p1, 3), strict = TRUE), msg) # pre-split
  expect_error(schedule_sampling(model, time = 10, list(p1, 3), strict = TRUE), msg) # post-removal
})

test_that("sampling in the same generation of the split is prevented (forward)", {
  p1 <- population(name = "p1", time = 1, N = 1, remove = 50)
  model <- compile_model(populations = p1, path = tempdir(), generation_time = 25, simulation_length = 100, overwrite = TRUE, force = TRUE)
  expect_warning(schedule_sampling(model, time = 3, list(p1, 3)), "No valid sampling")
})

test_that("sampling in the same generation of the split is prevented (backward)", {
  p1 <- population(name = "p1", time = 100, N = 1, remove = 10)
  model <- compile_model(populations = p1, path = tempdir(), generation_time = 25, overwrite = TRUE, force = TRUE)
  expect_warning(schedule_sampling(model, time = 97, list(p1, 3)), "No valid sampling")
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
  skip_if(!is_slendr_env_present())

  n_samples <- 5
  times <- c(10, 100)
  locations <- list(c(50, 50))
  simulation_length <- 100
  locations_file <- tempfile(fileext = ".gz")

  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", 1, 100, map = map, center = c(50, 50), radius = 50)
  model <- compile_model(pop, path = tempdir(), generation_time = 1,
                   competition = 10, mating = 10, dispersal = 10,
                   simulation_length = simulation_length, resolution = 1, overwrite = TRUE, force = TRUE)

  samples <- schedule_sampling(model, times = times, locations = locations, list(pop, n_samples))
  ts <- slim(model, sequence_length = 1, recombination_rate = 0, samples = samples,
       method = "batch", locations = locations_file, verbose = FALSE)

  # load the locations of all individuals throughout the simulation, and filter
  # down to `n_sample` of the ones closest to the sampling location [50, 50] in
  # each time point (essentially replicating what we're doing on the SLiM
  # backend during the simulation run)
  locations <- locations_file %>%
    readr::read_tsv(show_col_types = FALSE) %>%
    dplyr::mutate(time = simulation_length - gen + 1) %>%
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
  individuals <- ts_nodes(ts) %>%
    dplyr::filter(sampled) %>%
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
  skip_if(!is_slendr_env_present())

  n_samples <- 5
  times <- c(10, 100)
  locations <- list(c(0, 0), c(100, 100))
  simulation_length <- 100
  locations_file <- tempfile(fileext = ".gz")

  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", 1, 100, map = map, center = c(50, 50), radius = 50)
  model <- compile_model(pop, path = tempdir(), generation_time = 1,
                   competition = 10, mating = 10, dispersal = 10,
                   simulation_length = simulation_length, resolution = 1, overwrite = TRUE, force = TRUE)

  samples <- rbind(
    schedule_sampling(model, times = times[1], locations = locations[1], list(pop, n_samples)),
    schedule_sampling(model, times = times[2], locations = locations[2], list(pop, n_samples))
  )

  ts <- slim(model, sequence_length = 1, recombination_rate = 0, samples = samples,
       method = "batch", locations = locations_file, verbose = FALSE)

  # load the locations of all individuals throughout the simulation, and filter
  # down to `n_sample` of the ones closest to the sampling location [50, 50] in
  # each time point (essentially replicating what we're doing on the SLiM
  # backend during the simulation run)
  all_locations <- locations_file %>%
    readr::read_tsv(show_col_types = FALSE) %>%
    dplyr::mutate(time = simulation_length - gen + 1)

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
  all_individuals <- ts_nodes(ts) %>%
    dplyr::filter(sampled) %>%
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

test_that("sampling locations may only be given for spatial models", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 100)
  p2 <- population(name = "p2", parent = p1, time = 20, N = 100, center = c(1, 1), radius = 10)
  model <- compile_model(populations = list(p1, p2), generation_time = 1,
                         resolution = 1, simulation_length = 1000,
                         competition = 0, mating = 1, dispersal = 10)
  expect_error(
    schedule_sampling(model, time = 35, list(p2, 4), locations = list(c(75, 25)), strict = TRUE),
    "Sampling locations may only be specified for a spatial model"
  )
})

test_that("a mix of spatial and non-spatial samplings is not allowed for a single population", {
  skip_if(!is_slendr_env_present())

  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  p1 <- population(name = "p1", time = 10, N = 100)
  p2 <- population(name = "p2", map = map, time = 20, N = 100, center = c(1, 1), radius = 10)
  suppressWarnings(
    model <- compile_model(populations = list(p1, p2), generation_time = 1,
                         resolution = 1, simulation_length = 1000,
                         competition = 0, mating = 1, dispersal = 10)
  )
  s1 <- schedule_sampling(model, time = 35, list(p1, 3), strict = TRUE)
  s2 <- schedule_sampling(model, time = 35, list(p2, 4), locations = list(c(75, 25)), strict = TRUE)
  # this gives error
  s3 <- schedule_sampling(model, time = 35, list(p2, 5), strict = TRUE)
  s <- rbind(s1, s2, s3)
  expect_error(
    slim(model, samples = s, sequence_length = 1000, recombination_rate = 0),
    "For each population, samples must be all spatial or all non-spatial.\nThis is not true for the following populations: p2"
  )
  # this passes
  s3 <- schedule_sampling(model, time = 35, list(p2, 5), locations = list(c(10, 15)), strict = TRUE)
  s <- rbind(s1, s2, s3)
  expect_s3_class(slim(model, samples = s, sequence_length = 1000, recombination_rate = 0), "slendr_ts")
})

test_that("sampling table is correctly adjusted after simplification (msprime)", {
  skip_if(!is_slendr_env_present())

  pop1 <- population("pop1", time = 1, N = 100)
  pop2 <- population("pop2", time = 10, N = 42, parent = pop1)
  model <- compile_model(list(pop1, pop2), generation_time = 1, simulation_length = 1000, direction = "forward")

  # original tree sequence can be saved and loaded with or without the model
  tmp_big <- tempfile()
  ts_big <- msprime(model, sequence_length = 1000, recombination_rate = 0)
  ts_save(ts_big, tmp_big)
  expect_s3_class(ts_big_model <- ts_load(tmp_big, model), "slendr_ts")
  expect_s3_class(ts_big_nomodel <- ts_load(tmp_big), "slendr_ts")
  expect_true(nrow(ts_samples(ts_big_model)) == 142)
  expect_error(ts_samples(ts_big_nomodel), "Sampling schedule can only be extracted")

  # 'simple' simplified tree sequence can be saved and loaded with or without the model
  tmp_small1 <- tempfile()
  suppressWarnings(ts_simplify(ts_big) %>% ts_save(tmp_small1))
  expect_s3_class(ts_small1_model <- ts_load(tmp_small1, model), "slendr_ts")
  expect_s3_class(ts_small1_nomodel <- ts_load(tmp_small1), "slendr_ts")
  expect_true(nrow(ts_samples(ts_small1_model)) == 142)
  expect_error(ts_samples(ts_small1_nomodel), "Sampling schedule can only be extracted")

  # tree sequence simplified to a subset can be saved and loaded with or without the model
  tmp_small2 <- tempfile()
  ts_simplify(ts_big, simplify_to = c("pop1_1", "pop1_2", "pop1_3", "pop2_5")) %>% ts_save(tmp_small2)
  expect_s3_class(ts_small2_model <- ts_load(tmp_small2, model), "slendr_ts")
  expect_s3_class(ts_small2_nomodel <- ts_load(tmp_small2), "slendr_ts")
  expect_true(nrow(ts_samples(ts_small2_model)) == 4)
  expect_error(ts_samples(ts_small2_nomodel), "Sampling schedule can only be extracted")
})

test_that("sampling table is correctly adjusted after simplification (SLiM)", {
  skip_if(!is_slendr_env_present())

  pop1 <- population("pop1", time = 1, N = 100)
  pop2 <- population("pop2", time = 10, N = 42, parent = pop1)
  model <- compile_model(list(pop1, pop2), generation_time = 1, simulation_length = 1000, direction = "forward")

  # original tree sequence can be saved and loaded with or without the model
  tmp_big <- tempfile()
  ts_big <- slim(model, sequence_length = 1000, recombination_rate = 0) %>% ts_recapitate(recombination_rate = 0, Ne = 100)
  ts_save(ts_big, tmp_big)
  expect_s3_class(ts_big_model <- ts_load(tmp_big, model), "slendr_ts")
  expect_s3_class(ts_big_nomodel <- ts_load(tmp_big), "slendr_ts")
  expect_true(nrow(ts_samples(ts_big_model)) == 142)
  expect_error(ts_samples(ts_big_nomodel), "Sampling schedule can only be extracted")

  # 'simple' simplified tree sequence can be saved and loaded with or without the model
  tmp_small1 <- tempfile()
  suppressWarnings(ts_simplify(ts_big) %>% ts_save(tmp_small1))
  expect_s3_class(ts_small1_model <- ts_load(tmp_small1, model), "slendr_ts")
  expect_s3_class(ts_small1_nomodel <- ts_load(tmp_small1), "slendr_ts")
  expect_true(nrow(ts_samples(ts_small1_model)) == 142)
  expect_error(ts_samples(ts_small1_nomodel), "Sampling schedule can only be extracted")

  # tree sequence simplified to a subset can be saved and loaded with or without the model
  tmp_small2 <- tempfile()
  ts_simplify(ts_big, simplify_to = c("pop1_1", "pop1_2", "pop1_3", "pop2_5")) %>% ts_save(tmp_small2)
  expect_s3_class(ts_small2_model <- ts_load(tmp_small2, model), "slendr_ts")
  expect_s3_class(ts_small2_nomodel <- ts_load(tmp_small2), "slendr_ts")
  expect_true(nrow(ts_samples(ts_small2_model)) == 4)
  expect_error(ts_samples(ts_small2_nomodel), "Sampling schedule can only be extracted")
})
