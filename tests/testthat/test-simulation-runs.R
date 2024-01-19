skip_if(!is_slendr_env_present())
skip_if(slim_is_missing())

# Run a given forward/backward simulation with different combinations
# of start-end and burnin parameters
simulation_run <- function(direction, start, burnin, gen_time, simulation_length = NULL,
                           method = "batch", verbose = FALSE) {
  map <- world(xrange = c(0, 10), yrange = c(0, 10), landscape = "blank")
  p <- population("pop", N = 5, time = start, map = map, center = c(5, 5), radius = 1)
  model <- compile_model(p, direction = direction, simulation_length = simulation_length, path = tempdir(),
                         generation_time = gen_time, resolution = 1,
                         competition = 10, mating = 10, dispersal = 10,
                         overwrite = TRUE, force = TRUE)
  locations_file <- tempfile(fileext = ".gz")
  ts <- slim(model, burnin = burnin, sequence_length = 1, recombination_rate = 0,
       locations = locations_file, verbose = verbose, method = method)

  df <- suppressMessages(readr::read_tsv(locations_file, progress = FALSE)) %>%
    dplyr::mutate(time = convert_slim_time(gen, model)) %>%
    dplyr::filter(time >= 0)

  if (direction == "forward")
    df <- dplyr::arrange(df, time)
  else
    df <- dplyr::arrange(df, -time)

  list(df, model, ts)
}

# forward simulations - generation time = 1 -------------------------------

test_that("Forward simulation from generation 1 has the correct length without burnin", {
  direction <- "forward"; start <- 1; simulation_length <- 5; burnin <- 0; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  model <- results[[2]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Forward simulation from generation 1 has the correct length with burnin", {
  direction <- "forward"; start <- 1; simulation_length <- 5; burnin <- 20; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  model <- results[[2]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Forward simulation from generation > 1 has the correct length without burnin", {
  direction <- "forward"; start <- 8; simulation_length <- 5; burnin <- 0; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  model <- results[[2]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Forward simulation from generation > 1 has the correct length with burnin", {
  direction <- "forward"; start <- 8; simulation_length <- 5; burnin <- 100; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

# backward simulations - generation time = 1 ------------------------------

test_that("Backward simulation has the correct length without burnin", {
  direction <- "backward"; start <- 5; burnin <- 0; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == 0))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == 0)
  expect_true(length(unique(result$time)) == round(start / gen_time) + 1)
})

test_that("Backward simulation has the correct length with burnin", {
  direction <- "backward"; start <- 5; burnin <- 20; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == 0))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == 0)
  expect_true(length(unique(result$time)) == round(start / gen_time) + 1)
})

test_that("Backward simulation of limited length has the correct length without burnin", {
  direction <- "backward"; start <- 5; simulation_length = 3; burnin <- 0; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start - simulation_length))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start - simulation_length)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Backward simulation of limited length has the correct length with burnin", {
  direction <- "backward"; start <- 5; simulation_length = 3; burnin <- 20; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start - simulation_length))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start - simulation_length)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

# forward simulations - generation time > 1 -------------------------------

test_that("Forward simulation from generation 1 has the correct length without burnin", {
  direction <- "forward"; start <- 20; simulation_length <- 50; burnin <- 0; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length - simulation_length %% gen_time))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length - simulation_length %% gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Forward simulation from generation 1 has the correct length with burnin", {
  direction <- "forward"; start <- 20; simulation_length <- 50; burnin <- 100; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length - simulation_length %% gen_time))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length - simulation_length %% gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Forward simulation from generation > 1 has the correct length without burnin", {
  direction <- "forward"; start <- 200; simulation_length <- 50; burnin <- 0; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length - simulation_length %% gen_time))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length - simulation_length %% gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Forward simulation from generation > 1 has the correct length with burnin", {
  direction <- "forward"; start <- 200; simulation_length <- 50; burnin <- 100; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start + simulation_length - simulation_length %% gen_time))

  expect_true(min(result$time) == start)
  expect_true(max(result$time) == start + simulation_length - simulation_length %% gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

# backward simulations - generation time > 1 ------------------------------

test_that("Backward simulation has the correct length without burnin", {
  direction <- "backward"; start <- 200; burnin <- 0; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == 0))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == 0)
  expect_true(length(unique(result$time)) == round(start / gen_time) + 1)
})

test_that("Backward simulation has the correct length with burnin", {
  direction <- "backward"; start <- 200; burnin <- 300; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == 0))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == 0)
  expect_true(length(unique(result$time)) == round(start / gen_time) + 1)
})

test_that("Backward simulation of limited length has the correct length without burnin", {
  direction <- "backward"; start <- 200; simulation_length = 50; burnin <- 0; gen_time <- 20
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start - simulation_length + simulation_length %% gen_time))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start - simulation_length + simulation_length %% gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Backward simulation of limited length has the correct length with burnin", {
  direction <- "backward"; start <- 5; simulation_length = 3; burnin <- 20; gen_time <- 1
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start - simulation_length + simulation_length %% gen_time))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start - gen_time * round(simulation_length / gen_time))
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

# backward simulations - generation time > 1 (times not multiples) --------

test_that("Backward simulation has the correct length without burnin", {
  direction <- "backward"; start <- 200; burnin <- 0; gen_time <- 30
  results <- simulation_run(direction, start, burnin, gen_time)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start %% gen_time))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start %% gen_time)
  expect_true(length(unique(result$time)) == round(start / gen_time))
})

test_that("Backward simulation has the correct length with burnin", {
  direction <- "backward"; start <- 200; burnin <- 300; gen_time <- 30
  results <- simulation_run(direction, start, burnin, gen_time)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start %% gen_time))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start %% gen_time)
  expect_true(length(unique(result$time)) == round(start / gen_time))
})

test_that("Backward simulation of limited length has the correct length without burnin", {
  direction <- "backward"; start <- 200; simulation_length = 50; burnin <- 0; gen_time <- 30
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start - gen_time * (simulation_length %/% gen_time)))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start - gen_time * (simulation_length %/% gen_time) - gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})

test_that("Backward simulation of limited length has the correct length with burnin", {
  direction <- "backward"; start <- 200; simulation_length = 50; burnin <- 70; gen_time <- 30
  results <- simulation_run(direction, start, burnin, gen_time, simulation_length)
  result <- results[[1]]
  ts <- results[[3]]

  samples <- ts_samples(ts)
  expect_true(nrow(samples) == 5)
  expect_true(all(samples$time == start - gen_time * (simulation_length %/% gen_time)))

  expect_true(max(result$time) == start)
  expect_true(min(result$time) == start - gen_time * (simulation_length %/% gen_time) - gen_time)
  expect_true(length(unique(result$time)) == round(simulation_length / gen_time) + 1)
})
