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
