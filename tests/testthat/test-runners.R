skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

pop <- population("pop", time = 1, N = 10)
model <- compile_model(pop, generation_time = 1, direction = "forward", simulation_length = 10)

test_that("slim() returns a tree-sequence object by default", {
  result <- slim(model, sequence_length = 1, recombination_rate = 0)
  expect_s3_class(result, "slendr_ts")
  expect_s3_class(result, "tskit.trees.TreeSequence")
})

test_that("msprime() returns a tree-sequence object by default", {
  result <- msprime(model, sequence_length = 1, recombination_rate = 0)
  expect_s3_class(result, "slendr_ts")
  expect_s3_class(result, "tskit.trees.TreeSequence")
})

test_that("slim() does not return a tree sequence when this is not requested", {
  ts_file <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  expect_silent(slim(model, ts = ts_file, sequence_length = 1, recombination_rate = 0, load = FALSE))
})

test_that("msprime() does not return a tree sequence when this is not requested", {
  ts_file <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  expect_silent(msprime(model, ts = ts_file, sequence_length = 1, recombination_rate = 0, load = FALSE))
})

test_that("msprime() gives a warning when no output path is given and no tree sequence is to be loaded", {
  expect_warning(msprime(model, sequence_length = 1, recombination_rate = 0, load = FALSE),
                 "No custom tree-sequence output path is given")
})
