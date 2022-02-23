skip_if(!env_present("automatic_slendr_python_env")); setup_env(quiet = TRUE)

map <- world(xrange = c(1, 100), yrange = c(1, 100), landscape = "blank")

pop <- population("POP", time = 1, N = 100, center = c(50, 50), radius = 50, map = map)

model <- compile(
  populations = pop,
  generation_time = 1,
  sim_length = 100,
  resolution = 1,
  competition_dist = 10, mate_dist = 2, dispersal_dist = 1,
  overwrite = TRUE
)

samples <- sampling(model, times = 100, list(pop, 10))

slim(
  model, sampling = samples,
  sequence_length = 100000, recombination_rate = 1e-8,
  method = "batch",
  random_seed = 42
)

test_that("ape phylo conversion only works on simplified, coalesced trees", {
  ts1 <- ts_load(model)
  suppressWarnings(ts2 <- ts_load(model, simplify = TRUE))
  ts3 <- ts_load(model, recapitate = TRUE, Ne = 100, recombination_rate = 1e-8)

  expect_error(
    ts_phylo(ts1, 1, mode = "index", quiet = TRUE),
    "A tree sequence tree which is not fully coalesced"
  )
  expect_error(
    ts_phylo(ts2, 1, mode = "index", quiet = TRUE),
    "A tree sequence tree which is not fully coalesced"
  )
  expect_error(
    ts_phylo(ts3, 1, mode = "index", quiet = TRUE),
    "Please simplify your tree sequence down"
  )
})

ts <- ts_load(model, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
              Ne = 1000, recombination_rate = 1e-8, mutation_rate = 0.0000001)

test_that("ape phylo and tskit.Tree objects are created by ts_tree/ts_phylo", {
  t1 <- ts_tree(ts, 1, mode = "index")
  # we deal with the "not all nodes are spatial" warninng below
  suppressWarnings(t2 <- ts_phylo(ts, 1, mode = "index", quiet = TRUE))
  expect_s3_class(t1, "tskit.trees.Tree")
  expect_s3_class(t2, "phylo")
})

test_that("ape phylo and tskit.Tree objects are equivalent", {
  t1 <- ts_tree(ts, 1, mode = "index")
  suppressWarnings(t2 <- ts_phylo(ts, 1, mode = "index", quiet = TRUE))

  expect_true(t1$num_edges == nrow(t2$edge))
  expect_true(t1$num_samples() == length(t2$tip.label))

  t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
  t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
  t1_nodes <- c(t1_internal, t1_leaves)

  t2_nodes <- unique(as.vector(t2$edge))

  expect_true(length(t1_nodes) == length(t2_nodes))
})

test_that("ts_data only works on slendr_ts and phylo objects", {
  i <- 1

  t1 <- ts_tree(ts, i, mode = "index")
  suppressWarnings(t2 <- ts_phylo(ts, i, mode = "index", quiet = TRUE))

  t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
  t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
  t1_nodes <- c(t1_internal, t1_leaves)

  t2_nodes <- unique(as.vector(t2$edge))

  data <- ts_data(t2)

  expect_true(nrow(data) == t2$Nnode + length(t2$tip.label))
  expect_true(nrow(data) == length(intersect(t2_nodes, data$phylo_id)))
})

test_that("ts_data output contains the correct information for a given phylo tree", {
  for (i in seq_len(ts$num_trees)) {
    t1 <- ts_tree(ts, i, mode = "index")
    suppressWarnings(t2 <- ts_phylo(ts, i, mode = "index", quiet = TRUE))

    t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
    t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
    t1_nodes <- c(t1_internal, t1_leaves)

    t2_nodes <- unique(as.vector(t2$edge))

    data <- ts_data(t2)

    expect_true(nrow(data) == t2$Nnode + length(t2$tip.label))
    expect_true(nrow(data) == length(intersect(t2_nodes, data$phylo_id)))
  }
})

test_that("ts_phylo refuses a non-coalesced tree sequence", {
  ts <- ts_load(model)
  expect_error(ts_phylo(ts, 1), "A tree sequence tree which is not fully coalesced")
})

test_that("ts_phylo errors on a not-yet-simplified tree sequence", {
  ts <- ts_load(model, recapitate = TRUE, Ne = 10, recombination_rate = 0)
  expect_error(ts_phylo(ts, 1, quiet = TRUE), "Please simplify your tree sequence")
  ts <- ts_load(model, recapitate = TRUE, Ne = 10, recombination_rate = 0, simplify = TRUE)
  expect_s3_class(suppressWarnings(ts_phylo(ts, 1, quiet = TRUE)), "slendr_phylo")
})

test_that("ts_phylo gives a warning when a tree sequence is not fully spatial", {
  ts <- ts_load(model, recapitate = TRUE, Ne = 10, recombination_rate = 0, simplify = TRUE)
  expect_warning(ts_phylo(ts, 1, quiet = TRUE),
                 "Not all nodes have a known spatial location.")

  pop2 <- population("POP", time = 1, N = 10, center = c(50, 50), radius = 50, map = map)

  model2 <- compile(populations = pop2, generation_time = 1, sim_length = 1000,
    resolution = 1, competition_dist = 10, mate_dist = 50, dispersal_dist = 1)

  slim(model2, sequence_length = 1, recombination_rate = 0, method = "batch", random_seed = 42 )

  ts <- ts_load(model2, simplify = TRUE)
  expect_s3_class(ts_data(ts_phylo(ts, 1, quiet = TRUE)), "sf")
  expect_s3_class(attr(ts_phylo(ts, 1, quiet = TRUE), "branches"), "sf")
})
