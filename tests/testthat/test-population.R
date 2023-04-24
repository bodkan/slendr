#
# population calls
#
test_that("non-positive population time triggers an error", {
  expect_error(population("pop", time = -42, N = 100), "Split time must be a non-negative number")
  expect_s3_class(population("pop", time = 42, N = 100), "slendr_pop")
})

test_that("non-integer population split time is rounded", {
  pop <- population("pop", time = 41.6, N = 100)
  expect_true(attr(pop, "history")[[1]]$time %% 1 == 0)
  expect_true(attr(pop, "history")[[1]]$time == 42)
})

test_that("non-positive population size triggers an error", {
  expect_error(population("pop", time = 42, N = -100), "Population size must be a non-negative number")
  expect_s3_class(population("pop", time = 42, N = 100), "slendr_pop")
})

test_that("non-integer population split time is rounded", {
  pop <- population("pop", time = 42, N = 100.9)
  expect_true(attr(pop, "history")[[1]]$N %% 1 == 0)
  expect_true(attr(pop, "history")[[1]]$N == 101)
})

#
# population resizes (step)
#

test_that("non-integer population size is rounded (step resize)", {
  pop <- population("pop", time = 42, N = 100) %>%
    resize(N = 1000.9, how = "step", time = 100)
  expect_true(attr(pop, "history")[[2]]$N %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$N == 1001)
})

test_that("non-integer population resize time is rounded (step resize)", {
  pop <- population("pop", time = 42, N = 100) %>%
    resize(N = 1000, how = "step", time = 100.9)
  expect_true(attr(pop, "history")[[2]]$tresize %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$tresize == 101)
})

#
# population resizes (exponential)
#

test_that("non-integer population size is rounded (exponential resize)", {
  pop <- population("pop", time = 42, N = 100) %>%
    resize(N = 1000.9, how = "exponential", time = 100, end = 400)
  expect_true(attr(pop, "history")[[2]]$N %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$N == 1001)
})

test_that("non-integer population resize time is rounded (exponential resize)", {
  # start time non-integer
  pop <- population("pop", time = 42, N = 100) %>%
    resize(N = 1000, how = "exponential", time = 100.9, end = 400)
  expect_true(attr(pop, "history")[[2]]$tresize %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$tresize == 101)

  # end time non-integer
  pop <- population("pop", time = 42, N = 100) %>%
    resize(N = 1000, how = "exponential", time = 100, end = 400.9)
  expect_true(attr(pop, "history")[[2]]$tend %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$tend == 401)

  # start and end time non-integer
  pop <- population("pop", time = 42, N = 100) %>%
    resize(N = 1000, how = "exponential", time = 100.9, end = 400.9)
  expect_true(attr(pop, "history")[[2]]$tresize %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$tresize == 101)
  expect_true(attr(pop, "history")[[2]]$tend %% 1 == 0)
  expect_true(attr(pop, "history")[[2]]$tend == 401)
})

test_that("only strings fitting the requirements of valid Python identifiers can be names", {
  error_msg <- "A population name must be a character scalar value which must also be"

  valid_names <- list(
    "valid_identifier",
    "ValidIdentifier",
    "_another_valid1",
    "identifierWithÜmlaut",
    "αλφαβητικός",
    "متغير_عربي",
    "변수_한글"
  )

  invalid_names <- list(
    "1invalid_identifier",
    "identifier-with-hyphen",
    "αλφα-βητικός",
    "3متغير_عربي",
    "123변수_한글",
    c("qwe", "asd")
  )

  for (n in invalid_names) {
    expect_error(population(n, time = 1000, N = 100), error_msg)
  }

  skip_if(!is_slendr_env_present())
  init_env(quiet = TRUE)

  # msprime passes
  for (n in valid_names) {
    expect_s3_class(pop <- population(n, time = 1000, N = 100), "slendr_pop")
    model <- compile_model(pop, generation_time = 100, direction = "backward", serialize = FALSE)
    expect_s3_class(msprime(model, sequence_length = 1000, recombination_rate = 0), "slendr_ts")
  }

  # slim passes
  skip_if(Sys.which("slim") == "")
  for (n in valid_names) {
    expect_s3_class(pop <- population(n, time = 1000, N = 100), "slendr_pop")
    model <- compile_model(pop, generation_time = 100, direction = "backward")
    expect_s3_class(slim(model, sequence_length = 1000, recombination_rate = 0), "slendr_ts")
  }
})
