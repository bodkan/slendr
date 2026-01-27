skip_on_cran()
skip_if(!check_dependencies(python = TRUE))
init_env(quiet = TRUE)

pop <- population("pop", N = 1000, time = 1)
model <- compile_model(list(pop), generation_time = 1, direction = "forward", simulation_length = 1000, serialize = FALSE)

schedule <- schedule_sampling(model, times = 1001, list(pop, 10))

test_that("number of iterations must be a non-negative, non-zero integer", {
  error_msg <- "Iterations number must be a non-negative, non-zero integer"
  ts <- msprime(model, samples = schedule, sequence_length = 10e6, recombination_rate = 1e-8)
  expect_error(ts_ext1 <- ts_extend(ts, iterations = -10), error_msg)
  expect_error(ts_ext1 <- ts_extend(ts, iterations = 0), error_msg)
  expect_error(ts_ext1 <- ts_extend(ts, iterations = "heyo"), error_msg)
  expect_s3_class(ts_ext1 <- ts_extend(ts, iterations = 10), "slendr_ts")
  ts_ext2 <- ts_read(ts$extend_haplotypes(10), model = model)
})

test_that("ts_extend() gives the same tables as the Python method extend_haplotypes()", {
  ts <- msprime(model, samples = schedule, sequence_length = 10e6, recombination_rate = 1e-8, coalescent_only = FALSE)

  ts_ext1 <- ts_extend(ts, iterations = 10)
  ts_ext2 <- ts_read(ts$extend_haplotypes(10), model = model)

  expect_true(all.equal(ts_table(ts_ext1, "edges"), ts_table(ts_ext2, "edges")))
})
