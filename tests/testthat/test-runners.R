skip_if(!check_dependencies(python = TRUE))
init_env(quiet = TRUE)

pop <- population("pop", time = 1, N = 10)
model <- compile_model(pop, generation_time = 1, direction = "forward", simulation_length = 10)

test_that("slim() returns a tree-sequence object by default", {
  result <- slim(model, sequence_length = 1, recombination_rate = 0)
  expect_s3_class(result, "slendr_ts")
  expect_s3_class(result, "tskit.trees.TreeSequence")
})

test_that("msprime() returns a tree-sequence object by default", {
  result <- msprime(model, sequence_length = 1, recombination_rate = 0, random_seed = 123)
  expect_s3_class(result, "slendr_ts")
  expect_s3_class(result, "tskit.trees.TreeSequence")
})

test_that("if `path =` is given, msprime returns it back and saves a tree-sequence file", {
  path <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  result1 <- msprime(model, sequence_length = 1, recombination_rate = 0, path = path, random_seed = 123)
  ts_path <- file.path(path, "msprime.trees")
  expect_true(file.exists(ts_path))

  result2 <- msprime(model, sequence_length = 1, recombination_rate = 0, random_seed = 123)
  ts <- ts_read(file = ts_path, model)
  expect_equal(ts_nodes(ts), ts_nodes(result2))
})

test_that("SLiMgui does not start in an interactive session", {
  skip_if(!interactive())
  expect_error(slim(model, sequence_length = 1, recombination_rate = 0, method = "gui"),
               "SLiMgui can only be run from an interactive R session")
})

test_that("slendr model directory must be present", {
  broken_model <- model
  broken_model$path <- "nope"
  expect_error(slim(broken_model, sequence_length = 1, recombination_rate = 0),
               "Model directory 'nope' does not exist")
})

test_that("slendr model directory must be present", {
  broken_path <- "nope"
  expect_error(
    slim(model, sequence_length = 1, recombination_rate = 0, slim_path = broken_path),
    paste0("SLiM binary not found at ", broken_path)
  )
})
