skip_if(!env_present("automatic_slendr_python_env")); setup_env(quiet = TRUE)

map <- world(xrange = c(1, 100), yrange = c(1, 100), landscape = "blank")

pop <- population("POP", time = 1, N = 100, center = c(50, 50), radius = 50, map = map)

model <- compile(
  populations = pop,
  generation_time = 1,
  sim_length = 10,
  resolution = 1,
  competition_dist = 10, mate_dist = 2, dispersal_dist = 1,
  overwrite = TRUE
)

samples <- sampling(model, times = 100, list(pop, 10))

slim(
  model, sampling = samples,
  sequence_length = 1000, recombination_rate = 0,
  method = "batch",
  random_seed = 42
)

test_that("ape phylo conversion only works on simplified, coalesced trees", {
  ts1 <- ts_load(model)
  suppressWarnings(ts2 <- ts_load(model, simplify = TRUE))
  ts3 <- ts_load(model, recapitate = TRUE, Ne = 100, recombination_rate = 0)

  expect_error(
    ts_phylo(ts1, 1, mode = "index", quiet = TRUE),
    "The tree sequence must be fully coalesced or recapitated"
  )
  expect_error(
    ts_phylo(ts2, 1, mode = "index", quiet = TRUE),
    "The tree sequence must be fully coalesced or recapitated"
  )
  expect_error(
    ts_phylo(ts3, 1, mode = "index", quiet = TRUE),
    "Please simplify your tree sequence down to sampled individuals first"
  )
})

test_that("ape phylo conversion works as expected", {
  ts <- ts_load(model, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
                Ne = 1000, recombination_rate = 0, mutation_rate = 0.0000001)

  t1 <- ts_tree(ts, 1, mode = "index")
  t2 <- ts_phylo(ts, 1, mode = "index", quiet = TRUE)
  expect_s3_class(t1, "tskit.trees.Tree")
  expect_s3_class(t2, "phylo")
})
