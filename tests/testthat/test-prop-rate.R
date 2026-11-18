# This is only a very shallow testing of the proportion vs migration rate
# alternatives. More thorough testing happens in test-msprime-geneflow.R.

skip_if(!check_dependencies(python = TRUE, slim = TRUE))
init_env()

p1 <- population("p1", time = 1, N = 100)
p2 <- population("p2", time = 100, N = 100, parent = p1)

test_that("slim() runs properly handle gene-flow proportions", {
  gf1 <- gene_flow(p1, p2, start = 200, end = 210, proportion = 0.1)
  model1 <- compile_model(list(p1, p2), simulation_length = 1000, generation_time = 1, gene_flow = gf1)

  gf2 <- gene_flow(p1, p2, start = 200, end = 210, migration_rate = 0.01)
  model2 <- compile_model(list(p1, p2), simulation_length = 1000, generation_time = 1, gene_flow = gf2)

  # check that the two respective columns have the correct values
  expect_true(model1$geneflow$proportion == 0.1)
  expect_true(model1$geneflow$migration_rate == -1)
  expect_true(model2$geneflow$proportion == -1)
  expect_true(model2$geneflow$migration_rate == 0.01)

  log1 <- suppressMessages(capture.output(slim(model1, sequence_length = 1, recombination_rate = 0, verbose = TRUE)))
  expect_true(sum(grepl("0.1\\% over 10 generations", log1)) == 1)

  log2 <- suppressMessages(capture.output(slim(model2, sequence_length = 1, recombination_rate = 0, verbose = TRUE)))
  expect_true(sum(grepl("0.01 per generation", log2)) == 1)
})
