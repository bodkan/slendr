skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

test_that("only serialized models can be run on the command line", {
  pop1 <- population("pop1", N = 1000, time = 1)
  pop2 <- population("pop2", N = 1000, time = 2, parent = pop1)

  model <- compile_model(list(pop1, pop2), generation_time = 1, direction = "forward", simulation_length = 1000, serialize = FALSE)

  out <- tempfile()
  expect_error(
    msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, output = out, run = FALSE),
    "Impossible to run a non-serialized slendr model on the command line"
  )
})

test_that("msprime command run manually on the command line give the correct output", {
  pop1 <- population("pop1", N = 1000, time = 1)
  pop2 <- population("pop2", N = 1000, time = 2, parent = pop1)

  model <- compile_model(list(pop1, pop2), generation_time = 1, direction = "forward", simulation_length = 1000)

  # check that a simulated tree-sequence file is where it's supposed to be if the
  # model is run on the CLI
  out <- tempfile()
  out_cmd <- capture.output(
    cmd <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, output = out, run = FALSE, random_seed = 42))
  system(cmd, ignore.stdout = TRUE)
  ts_manual <- ts_load(out, model = model)
  expect_s3_class(ts_manual, "slendr_ts")

  # check that the manually simulated tree-sequence matches what comes from running inside slendr
  ts_r <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42)
  expect_equal(ts_nodes(ts_manual), ts_nodes(ts_r))

  # and check that the command itself matches
  expect_equal(gsub(" *$", "", out_cmd[4]), gsub(" *$", "", cmd))
})

test_that("slim command run manually on the command line give the correct output", {
  pop1 <- population("pop1", N = 1000, time = 1)
  pop2 <- population("pop2", N = 1000, time = 2, parent = pop1)

  model <- compile_model(list(pop1, pop2), generation_time = 1, direction = "forward", simulation_length = 1000)

  # check that a simulated tree-sequence file is where it's supposed to be if the
  # model is run on the CLI
  out <- tempfile()
  out_cmd <- capture.output(
    cmd <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8, output = out, run = FALSE, random_seed = 42))
  system(cmd, ignore.stdout = TRUE)
  ts_manual <- ts_load(out, model = model)
  expect_s3_class(ts_manual, "slendr_ts")

  # check that the manually simulated tree-sequence matches what comes from running inside slendr
  ts_r <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42)
  expect_equal(ts_nodes(ts_manual), ts_nodes(ts_r))

  # and check that the command itself matches
  expect_equal(
    gsub(" *$", "", out_cmd[-c(1:3, length(out_cmd) - 1, length(out_cmd))]),
    strsplit(cmd, "\n")[[1]]
  )
})

test_that("ensure that a model reaches full coalescence", {
  pop1 <- population("pop1", N = 1000, time = 1)
  pop2 <- population("pop2", N = 1000, time = 1)
  model <- compile_model(list(pop1, pop2), generation_time = 1, direction = "forward", simulation_length = 1000)

  expect_error(msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, verbose = TRUE),
               "Multiple ancestral populations without a common ancestor")

  pop3 <- population("pop2", N = 1000, time = 2, parent = pop1)
  model <- compile_model(list(pop1, pop3), generation_time = 1, direction = "forward", simulation_length = 1000)

  expect_s3_class(
    msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, verbose = FALSE),
    "slendr_ts"
  )
})

test_that("population size must be a non-negative number", {
  expect_error(pop <- population("asd", N = -1, time = 100),
               "Population size must be a non-negative number")
  expect_error(pop <- population("asd", N = 0, time = 100),
               "Population size must be a non-negative number")
  expect_s3_class(pop <- population("asd", N = 1, time = 100), "slendr_pop")
})

test_that("population time must be a non-negative number", {
  expect_error(pop <- population("asd", N = 100, time = -1),
               "Split time must be a non-negative number")
  expect_error(pop <- population("asd", N = 100, time = 0),
               "Split time must be a non-negative number")
  expect_s3_class(pop <- population("asd", N = 1, time = 100), "slendr_pop")
})
