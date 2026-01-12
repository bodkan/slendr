# In this script we test that all deprecated functions and arguments issue
# a proper warning for users

skip_if(!check_dependencies(python = TRUE))
init_env(quiet = TRUE)

test_that("ts_load() deprecated in favour of ts_read()", {
  expect_warning(ts_load(system.file("extdata/models/introgression_slim.trees", package = "slendr")))
})

test_that("ts_save() deprecated in favour of ts_write()", {
  ts <- ts_read(system.file("extdata/models/introgression_slim.trees", package = "slendr"))
  expect_warning(ts_save(ts, tempfile()))
})

pop1 <- population("pop1", N = 100, time = 100)
pop2 <- population("pop2", N = 100, time = 100)

test_that("using the rate argument gives a warning", {
  msg <- "The argument `rate` is about to be deprecated"
  expect_warning(gf <- gene_flow(from = pop1, to = pop2, start = 10, end = 0, rate = 0.1), msg)
  model <- compile_model(list(pop1, pop2), gene_flow = gf, generation_time = 1, simulation_length = 100)
  expect_true(model$geneflow$proportion[1] == 0.1)
})

test_that("not using either migration_rate or proportion arguments gives an error", {
  msg <- "Either `migration_rate` or `proportion` arguments must be\nspecified."
  expect_error(gene_flow(from = pop1, to = pop2, start = 10, end = 0), msg)
})

test_that("default gene-flow parameter is still the proportion", {
  gf <- gene_flow(from = pop1, to = pop2, start = 10, end = 0, 0.42)
  model <- compile_model(list(pop1, pop2), gene_flow = gf, generation_time = 1, simulation_length = 100)
  expect_true(model$geneflow$proportion[1] == 0.42)
})

test_that("rate argument is correctly distributed into an overall proportion", {
  tstart <- 10; tend <- 0; rate <- 0.01
  gf <- gene_flow(from = pop1, to = pop2, start = tstart, end = tend, migration_rate = rate)

  model <- compile_model(list(pop1, pop2), gene_flow = gf, generation_time = 1, simulation_length = 100)
  expect_true(model$geneflow$proportion[1] == abs(tend - tstart) * rate)
})
