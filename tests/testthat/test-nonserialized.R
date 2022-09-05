skip_if(!slendr:::check_env_present())
setup_env(quiet = TRUE)

ooa <- population("OOA", time = 30000, N = 50, remove = 25000)
ehg <- population("EHG", parent = ooa, time = 28000, N = 100, remove = 6000)
eur <- population("EUR", parent = ehg, time = 25000, N = 200) %>%
  resize(N = 10000, how = "exponential", time = 5000, end = 0)
ana <- population("ANA", time = 28000, N = 300, parent = ooa, remove = 40)

gf <- list(
  gene_flow(from = ana, to = yam, rate = 0.5, start = 6500, end = 6400),
  gene_flow(from = ana, to = eur, rate = 0.5, start = 8000, end = 6000)
)

test_that("non-serialized models are correctly simulated with msprime (but not SLiM)", {
  model <- compile_model(
    populations = list(ooa, ehg, eur, ana, yam),
    gene_flow = gf, generation_time = 30,
    serialize = FALSE
  )

  expect_true(is.null(model$path))
  expect_true(is.null(model$checksums))

  expect_error(slim(model, sequence_length = 1, recombination_rate = 0),
               "not possible to simulate non-serialized models")

  ts <- msprime(model, sequence_length = 100, recombination_rate = 0)
  expect_s3_class(ts, "slendr_ts")
  expect_s3_class(ts, "tskit.trees.TreeSequence")
})

test_that("non-serialized models give the same result as serialized models", {
  # create a serialized model
  model_ser <- compile_model(
    populations = list(ooa, ehg, eur, ana, yam),
    gene_flow = gf, generation_time = 30
  )
  expect_true(file.exists(model_ser$path))
  expect_true(is.data.frame(model_ser$checksums))

  # compile a non-serialized model
  model_nonser <- compile_model(
    populations = list(ooa, ehg, eur, ana, yam),
    gene_flow = gf, generation_time = 30,
    serialize = FALSE
  )
  expect_true(is.null(model_nonser$path))
  expect_true(is.null(model_nonser$checksums))

  expect_error(slim(model_nonser, sequence_length = 1, recombination_rate = 0),
               "not possible to simulate non-serialized models")
  expect_s3_class(slim(model_ser, sequence_length = 1, recombination_rate = 0), "slendr_ts")

  ts_nonser <- msprime(model_nonser, sequence_length = 10000, recombination_rate = 0) %>%
    ts_mutate(mutation_rate = 0.0001, random_seed = 42)
  expect_s3_class(ts_nonser, "slendr_ts")
  expect_s3_class(ts_nonser, "tskit.trees.TreeSequence")

  ts_ser <- msprime(model_nonser, sequence_length = 10000, recombination_rate = 0) %>%
    ts_mutate(mutation_rate = 0.0001, random_seed = 42)
  expect_s3_class(ts_ser, "slendr_ts")
  expect_s3_class(ts_ser, "tskit.trees.TreeSequence")

  # check equivalence of annotated tree-sequence tables
  expect_true(all(ts_nodes(ts_ser) == ts_nodes(ts_nonser), na.rm = TRUE))
  expect_true(all(ts_edges(ts_ser) == ts_edges(ts_nonser), na.rm = TRUE))

  # check equivalence of raw tree-sequence tables
  expect_true(all(ts_table(ts_ser, "individuals") == ts_table(ts_nonser, "individuals"), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser, "edges") == ts_table(ts_nonser, "edges"), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser, "nodes") == ts_table(ts_nonser, "nodes"), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser, "mutations") == ts_table(ts_nonser, "mutations"), na.rm = TRUE))

  # check equivalence of simplification for both tree sequences
  ts_ser_small <- ts_simplify(ts_ser, simplify_to = c("EUR_1", "ANA_1", "EHG_1"))
  ts_nonser_small <- ts_simplify(ts_nonser, simplify_to = c("EUR_1", "ANA_1", "EHG_1"))
  expect_true(all(ts_nodes(ts_ser_small) == ts_nodes(ts_nonser_small), na.rm = TRUE))
  expect_true(all(ts_edges(ts_ser_small) == ts_edges(ts_nonser_small), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser_small, "individuals") == ts_table(ts_nonser_small, "individuals"), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser_small, "edges") == ts_table(ts_nonser_small, "edges"), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser_small, "nodes") == ts_table(ts_nonser_small, "nodes"), na.rm = TRUE))
  expect_true(all(ts_table(ts_ser_small, "mutations") == ts_table(ts_nonser_small, "mutations"), na.rm = TRUE))

  # check equivalence of trees of both tree sequences
  tree_ser <- ts_phylo(ts_ser_small, 1, quiet = TRUE)
  tree_nonser <- ts_phylo(ts_nonser_small, 1, quiet = TRUE)
  expect_true(all(sapply(names(tree_ser), function(x) all(tree_ser[[x]] == tree_nonser[[x]]))))
})
