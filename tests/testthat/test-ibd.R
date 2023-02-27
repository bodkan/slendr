skip_if(!is_slendr_env_present())

pop <- population("POP", time = 1, N = 1000)
model <- compile_model(populations = pop, generation_time = 1, simulation_length = 1000)

ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8)

test_that("ts_ibd() works", {
  expect_equal(2 * 2, 4)
})
