skip_if(!check_dependencies(python = TRUE, slim = TRUE))
init_env()

p1 <- population("p1", time = 1, N = 100)
p2 <- population("p2", time = 100, N = 100, parent = p1)

test_that("slim() runs properly handle gene-flow proportions", {
  gf1 <- gene_flow(p1, p2, start = 200, end = 210, proportion = 0.1)
  model1 <- compile_model(list(p1, p2), simulation_length = 1000, generation_time = 1, gene_flow = gf1)
  model1$geneflow
  ts <- slim(model1, sequence_length = 1, recombination_rate = 0)
  expect_s3_class(ts, "slendr_ts")
})

test_that("slim() runs properly handle gene-flow migration rates", {
  gf2 <- gene_flow(p1, p2, start = 200, end = 210, migration_rate = 0.01)
  model2 <- compile_model(list(p1, p2), simulation_length = 1000, generation_time = 1, gene_flow = gf2)
  model2$geneflow
  ts <- slim(model2, sequence_length = 1, recombination_rate = 0)
  expect_s3_class(ts, "slendr_ts")
})
