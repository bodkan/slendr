skip_on_cran()
skip_if(!check_dependencies(python = TRUE))

map <- readRDS("map.rds")

calculate_exp_sizes <- function(N1, N2, t1, t2) {
  r <- log(N2 / N1) / abs(t2 - t1)
  N <- rev(round(N1 * exp(r * (abs(t2 - t1) - seq(0, abs(t2 - t1))))))
  N
}

test_that("Population size stays constant when specified", {
  pop <- population("pop", time = 1000, N = 100, map = map, center = c(20, 50), radius = 500e3)
  res <- run_sim(pop, "backward")
  start_N <- attr(pop, "history")[[1]]$N
  expect_true(all(res$N == start_N))
})

test_that("Population size is increased correctly in single step", {
  pop <- population("pop", time = 1000, N = 100, map = map, center = c(20, 50), radius = 500e3)
  res <- run_sim(resize(pop, N = 50, time = 900, how = "step"), "backward")
  start_N <- attr(pop, "history")[[1]]$N
  expect_true(dplyr::filter(res, time > 900) %>% { all(.$N == start_N) })
  expect_true(dplyr::filter(res, time <= 900) %>% { all(.$N == 50) })
})

test_that("Population size is increased correctly in two steps", {
  pop <- population("pop", time = 1000, N = 100, map = map, center = c(20, 50), radius = 500e3)
  res <- run_sim(
    resize(pop, N = 50, time = 900, how = "step") %>%
      resize(N = 500, time = 300, how = "step"), "backward")
  start_N <- attr(pop, "history")[[1]]$N
  expect_true(dplyr::filter(res, time > 900) %>% { all(.$N == start_N) })
  expect_true(dplyr::filter(res, time <= 900 & time > 300) %>% { all(.$N == 50) })
  expect_true(dplyr::filter(res, time < 300) %>% { all(.$N == 500) })
})

test_that("Simulated exponential growth matches theoretical expectations", {
  N1 <- 100; N2 <- 1000
  t1 <- 900; t2 <- 600

  pop <- population("pop", time = 1000, N = N1, map = map, center = c(20, 50), radius = 500e3)
  res <- run_sim(resize(pop, N = N2, time = t1, end = t2, how = "exponential"), "backward")
  expected_N <- calculate_exp_sizes(N1, N2, t1, t2)

  expect_true(res[1, ]$N == N1) # initial size
  expect_true(dplyr::filter(res, N > N1) %>% { .$time[1] == t1 - 1 }) # start of the growth
  expect_true(res[nrow(res), ]$N == N2) # final size

  # sizes match expectations
  expect_true(dplyr::filter(res, time <= t1 & time >= t2) %>% { all(.$N == expected_N) })
})

test_that("Simulated exponential shrinking matches theoretical expectations", {
  N1 <- 1000; N2 <- 300
  t1 <- 900; t2 <- 600

  pop <- population("pop", time = 1000, N = N1, map = map, center = c(20, 50), radius = 500e3)
  res <- run_sim(resize(pop, N = N2, time = t1, end = t2, how = "exponential"), "backward")
  expected_N <- calculate_exp_sizes(N1, N2, t1, t2)

  expect_true(res[1, ]$N == N1) # initial size
  expect_true(dplyr::filter(res, N < N1) %>% { .$time[1] == t1 - 1 }) # start of the growth
  expect_true(res[nrow(res), ]$N == N2) # final size

  # sizes match expectations
  expect_true(dplyr::filter(res, time <= t1 & time >= t2) %>% { all(.$N == expected_N) })
})

test_that("Multiple resize event types are allowed (backward model)", {
  N1 <- 100; N2 <- 1000; N3 <- 80
  t1 <- 900; t2 <- 600; t3 <- 200

  pop <- population("pop", time = 1000, N = N1, map = map, center = c(20, 50), radius = 500e3) %>%
    resize(N = N2, time = t1, end = t2, how = "exponential") %>%
    resize(N = N3, time = t3, how = "step")

  res <- run_sim(pop, "backward")
  expected_N <- calculate_exp_sizes(N1, N2, t1, t2)

  expect_true(res[1, ]$N == N1) # initial size
  expect_true(dplyr::filter(res, N > N1) %>% { .$time[1] == t1 - 1 }) # start of the exponential growth
  expect_true(dplyr::filter(res, N == N3) %>% { .$time[1] == t3 }) # time of the step resize event
  expect_true(res[nrow(res), ]$N == N3) # final size

  # sizes match expectations
  expect_true(dplyr::filter(res, time <= t1 & time >= t2) %>% { all(.$N == expected_N) })
})

test_that("Multiple resize event types are allowed (forward model)", {
  N1 <- 1000; N2 <- 300; N3 <- 80
  t1 <- 2; t2 <- 600; t3 <- 900

  pop <- population("pop", time = 1, N = N1, map = map, center = c(20, 50), radius = 500e3) %>%
    resize(N = N2, time = t1, end = t2, how = "exponential") %>%
    resize(N = N3, time = t3, how = "step")

  res <- run_sim(pop, "forward", simulation_length = 1000)
  expected_N <- calculate_exp_sizes(N1, N2, t1, t2)

  expect_true(res[1, ]$N == N1) # initial size
  expect_true(dplyr::filter(res, N < N1) %>% { .$time[1] == t1 + 1 }) # start of the exponential growth
  expect_true(dplyr::filter(res, N == N3) %>% { .$time[1] == t3 }) # time of the step resize event
  expect_true(res[nrow(res), ]$N == N3) # final size

  # sizes match expectations
  expect_true(dplyr::filter(res, time <= t1 & time >= t2) %>% { all(.$N == expected_N) })
})

test_that("Resize cannot happen at the same time as population split (forward)", {
  error_msg <- "Population resize cannot happen at the time the population is created"

  anc <- population("anc", time = 1, N = 1000)
  pop <- population("pop", time = 10, N = 1000, parent = anc)

  expect_error(resize(pop, time = 10, N = 100, how = "step"), error_msg)
  expect_error(resize(pop, time = 10, end = 20, N = 100, how = "exponential"), error_msg)

  expect_s3_class(resize(pop, time = 11, N = 100, how = "step"), "slendr_pop")
  resized_step <- resize(pop, time = 11, N = 100, how = "step")
  expect_true(attr(resized_step, "history")[[2]]$event == "resize")
  expect_true(is.na(attr(resized_step, "history")[[2]]$tend))

  expect_s3_class(resize(pop, time = 11, end = 20, N = 100, how = "exponential"), "slendr_pop")
  resized_exp <- resize(pop, time = 11, end = 20, N = 100, how = "exponential")
  expect_true(attr(resized_exp, "history")[[2]]$event == "resize")
  expect_true(attr(resized_exp, "history")[[2]]$tend == 20)
})

test_that("Resize cannot happen at the same time as population split (backward)", {
  error_msg <- "Population resize cannot happen at the time the population is created"

  anc <- population("anc", time = 1000, N = 1000)
  pop <- population("pop", time = 100, N = 1000, parent = anc)

  expect_error(resize(pop, time = 100, N = 100, how = "step"), error_msg)
  expect_error(resize(pop, time = 100, end = 20, N = 100, how = "exponential"), error_msg)

  expect_s3_class(resize(pop, time = 99, N = 100, how = "step"), "slendr_pop")
  resized_step <- resize(pop, time = 99, N = 100, how = "step")
  expect_true(attr(resized_step, "history")[[2]]$event == "resize")
  expect_true(is.na(attr(resized_step, "history")[[2]]$tend))

  expect_s3_class(resize(pop, time = 99, end = 20, N = 100, how = "exponential"), "slendr_pop")
  resized_exp <- resize(pop, time = 99, end = 20, N = 100, how = "exponential")
  expect_true(attr(resized_exp, "history")[[2]]$event == "resize")
  expect_true(attr(resized_exp, "history")[[2]]$tend == 20)
})
