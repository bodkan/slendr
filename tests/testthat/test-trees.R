skip_if(!is_slendr_env_present())

map <- world(xrange = c(1, 100), yrange = c(1, 100), landscape = "blank")

pop <- population("POP", time = 1, N = 100, center = c(50, 50), radius = 50, map = map)

model <- compile_model(
  populations = pop,
  generation_time = 1,
  simulation_length = 100,
  resolution = 1,
  competition = 10, mating = 2, dispersal = 1,
  overwrite = TRUE, force = TRUE
)

samples <- schedule_sampling(model, times = 100, list(pop, 10))

slim_ts <- tempfile(fileext = ".slim.trees")
msprime_ts <- tempfile(fileext = ".msprime.trees")

slim(
  model, samples = samples, output = slim_ts,
  sequence_length = 100000, recombination_rate = 1e-8,
  method = "batch",
  random_seed = 42
)

msprime(
  model, samples = samples, output = msprime_ts,
  sequence_length = 100000, recombination_rate = 1e-8,
  random_seed = 42
)


test_that("ape phylo conversion only works on simplified, coalesced trees (SLiM)", {
  ts1 <- ts_load(model = model, file = slim_ts)
  suppressWarnings(ts2 <- ts_load(model = model, file = slim_ts, simplify = TRUE))
  ts3 <- ts_load(model = model, file = slim_ts, recapitate = TRUE, Ne = 100, recombination_rate = 1e-8)

  expect_error(
    ts_phylo(ts1, 0, mode = "index", quiet = TRUE),
    "A tree sequence tree which is not fully coalesced"
  )
  expect_error(
    ts_phylo(ts2, 0, mode = "index", quiet = TRUE),
    "A tree sequence tree which is not fully coalesced"
  )
  expect_error(
    ts_phylo(ts3, 0, mode = "index", quiet = TRUE),
    "Please simplify your tree sequence first"
  )
})

test_that("ape phylo conversion only works on simplified, coalesced trees (msprime)", {
  ts1 <- ts_load(model = model, file = msprime_ts)
  suppressWarnings(ts2 <- ts_load(model = model, file = msprime_ts, simplify = TRUE))
  suppressWarnings(ts3 <- ts_load(model = model, file = msprime_ts, recapitate = TRUE, Ne = 100, recombination_rate = 1e-8))

  expect_s3_class(ts_phylo(ts1, 0, mode = "index", quiet = TRUE), "slendr_phylo")
  expect_s3_class(ts_phylo(ts2, 0, mode = "index", quiet = TRUE), "slendr_phylo")
  expect_s3_class(ts_phylo(ts3, 0, mode = "index", quiet = TRUE), "slendr_phylo")
})

ts1 <- ts_load(model = model, file = slim_ts, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
              Ne = 1000, recombination_rate = 1e-8, mutation_rate = 0.0000001)
ts2 <- ts_load(model = model, file = msprime_ts, mutation_rate = 0.0000001)

test_that("ape phylo and tskit.Tree objects are created by ts_tree/ts_phylo (SLiM)", {
  t1 <- ts_tree(ts1, 0, mode = "index")
  # we deal with the "not all nodes are spatial" warninng below
  suppressWarnings(t2 <- ts_phylo(ts1, 0, mode = "index", quiet = TRUE))
  expect_s3_class(t1, "tskit.trees.Tree")
  expect_s3_class(t2, "phylo")
})

test_that("ape phylo and tskit.Tree objects are created by ts_tree/ts_phylo (msprime)", {
  t1 <- ts_tree(ts2, 0, mode = "index")
  # we deal with the "not all nodes are spatial" warning below
  suppressWarnings(t2 <- ts_phylo(ts2, 0, mode = "index", quiet = TRUE))
  expect_s3_class(t1, "tskit.trees.Tree")
  expect_s3_class(t2, "phylo")
})

test_that("ape phylo and tskit.Tree objects are equivalent (SLiM)", {
  t1 <- ts_tree(ts1, 0, mode = "index")
  suppressWarnings(t2 <- ts_phylo(ts1, 0, mode = "index", quiet = TRUE))

  expect_true(t1$num_edges == nrow(t2$edge))
  expect_true(t1$num_samples() == length(t2$tip.label))

  t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
  t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
  t1_nodes <- c(t1_internal, t1_leaves)

  t2_nodes <- unique(as.vector(t2$edge))

  expect_true(length(t1_nodes) == length(t2_nodes))

  # even the recapitated nodes should have numerical time_tskit values
  # (after the fix of the join operation which caused NA to be introduced)
  expect_true(all(!is.na(ts_nodes(t2)$time_tskit)))
})


test_that("ape phylo and tskit.Tree objects are equivalent (msprime)", {
  t1 <- ts_tree(ts2, 0, mode = "index")
  suppressWarnings(t2 <- ts_phylo(ts2, 0, mode = "index", quiet = TRUE))

  expect_true(t1$num_edges == nrow(t2$edge))
  expect_true(t1$num_samples() == length(t2$tip.label))

  t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
  t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
  t1_nodes <- c(t1_internal, t1_leaves)

  t2_nodes <- unique(as.vector(t2$edge))

  expect_true(length(t1_nodes) == length(t2_nodes))
})

test_that("ts_nodes only works on slendr_ts and phylo objects (SLiM)", {
  i <- 1

  t1 <- ts_tree(ts1, i - 1, mode = "index")
  suppressWarnings(t2 <- ts_phylo(ts1, i - 1, mode = "index", quiet = TRUE))

  t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
  t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
  t1_nodes <- c(t1_internal, t1_leaves)

  t2_nodes <- unique(as.vector(t2$edge))

  data <- ts_nodes(t2)

  expect_true(nrow(data) == t2$Nnode + length(t2$tip.label))
  expect_true(nrow(data) == length(intersect(t2_nodes, data$phylo_id)))
})

test_that("ts_nodes only works on slendr_ts and phylo objects (msprime)", {
  i <- 1

  t1 <- ts_tree(ts2, i - 1, mode = "index")
  suppressWarnings(t2 <- ts_phylo(ts2, i - 1, mode = "index", quiet = TRUE))

  t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
  t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
  t1_nodes <- c(t1_internal, t1_leaves)

  t2_nodes <- unique(as.vector(t2$edge))

  data <- ts_nodes(t2)

  expect_true(nrow(data) == t2$Nnode + length(t2$tip.label))
  expect_true(nrow(data) == length(intersect(t2_nodes, data$phylo_id)))
})

test_that("ts_nodes output contains the correct information for a given phylo tree (SLiM)", {
  for (i in seq_len(ts1$num_trees)) {
    t1 <- ts_tree(ts1, i - 1, mode = "index")
    suppressWarnings(t2 <- ts_phylo(ts1, i - 1, mode = "index", quiet = TRUE))

    t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
    t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
    t1_nodes <- c(t1_internal, t1_leaves)

    t2_nodes <- unique(as.vector(t2$edge))

    data <- ts_nodes(t2)

    expect_true(nrow(data) == t2$Nnode + length(t2$tip.label))
    expect_true(nrow(data) == length(intersect(t2_nodes, data$phylo_id)))
  }
})

test_that("ts_nodes output contains the correct information for a given phylo tree (msprime)", {
  for (i in seq_len(ts2$num_trees)) {
    t1 <- ts_tree(ts2, i - 1, mode = "index")
    suppressWarnings(t2 <- ts_phylo(ts2, i - 1, mode = "index", quiet = TRUE))

    t1_internal <- t1$parent_array %>% .[. != -1] %>% unique()
    t1_leaves <- reticulate::iterate(t1$leaves(t1$root))
    t1_nodes <- c(t1_internal, t1_leaves)

    t2_nodes <- unique(as.vector(t2$edge))

    data <- ts_nodes(t2)

    expect_true(nrow(data) == t2$Nnode + length(t2$tip.label))
    expect_true(nrow(data) == length(intersect(t2_nodes, data$phylo_id)))
  }
})

test_that("ts_phylo refuses a non-coalesced tree sequence", {
  ts <- ts_load(model = model, file = slim_ts)
  expect_error(ts_phylo(ts, 0), "A tree sequence tree which is not fully coalesced")
})

test_that("ts_phylo errors on a not-yet-simplified tree sequence", {
  ts <- ts_load(model = model, file = slim_ts, recapitate = TRUE, Ne = 10, recombination_rate = 0)
  expect_error(ts_phylo(ts, 0, quiet = TRUE), "Please simplify your tree sequence")
  ts <- ts_load(model = model, file = slim_ts, recapitate = TRUE, Ne = 10, recombination_rate = 0, simplify = TRUE)
  expect_s3_class(suppressWarnings(ts_phylo(ts, 0, quiet = TRUE)), "slendr_phylo")
})

test_that("ts_phylo gives a warning when a tree sequence is not fully spatial", {
  ts <- ts_load(model = model, file = slim_ts, recapitate = TRUE, Ne = 10, recombination_rate = 0, simplify = TRUE)
  expect_warning(ts_phylo(ts, 0, quiet = TRUE),
                 "Not all nodes have a known spatial location.")

  pop2 <- population("POP", time = 1, N = 10, center = c(50, 50), radius = 50, map = map)

  model2 <- compile_model(populations = pop2, generation_time = 1, simulation_length = 1000,
    resolution = 1, competition = 10, mating = 50, dispersal = 1)

  ts <- slim(model2, sequence_length = 1, recombination_rate = 0, method = "batch", random_seed = 42 ) %>% ts_simplify()

  expect_s3_class(ts_nodes(ts_phylo(ts, 0, quiet = TRUE)), "sf")
  expect_s3_class(attr(ts_phylo(ts, 0, quiet = TRUE), "edges"), "sf")
})

test_that("ts_nodes and ts_edges give the same result in single-tree tree sequences", {
  ts <- slim(
    model, samples = samples,
    sequence_length = 100000, recombination_rate = 0,
    method = "batch",
    random_seed = 42
  ) %>%
    ts_recapitate(Ne = 100, recombination_rate = 0) %>%
    ts_simplify()
  suppressWarnings(tree <- ts_phylo(ts, 0, quiet = TRUE))

  nodes_tree <- ts_nodes(tree) %>% dplyr::arrange(time) %>% dplyr::select(-phylo_id)
  nodes_ts <- ts_nodes(ts) %>% dplyr::arrange(time)

  expect_true(all(nodes_ts == nodes_tree, na.rm = TRUE))
})
