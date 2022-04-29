skip_if(!slendr:::check_env_present())

set.seed(42)

simulate_slim_ts <- function(N) {
  script_file <- tempfile()
  ts_file <- tempfile()

  writeLines(sprintf('initialize() {
    setSeed(42);
  	initializeTreeSeq();
  	initializeMutationRate(0);
  	initializeMutationType("m1", 0.5, "f", 0.0);
  	initializeGenomicElementType("g1", m1, 1.0);
  	initializeGenomicElement(g1, 0, 1e8-1);
  	initializeRecombinationRate(1e-8);
  }
  1 {
  	sim.addSubpop("p1", %d);
  }
  4010 late() {
  	sim.treeSeqOutput("%s");
  }
  ', N, ts_file), script_file)
  system2("slim", script_file, stdout = FALSE)

  ts_file
}

simulate_msprime_ts <- function(N) {
  msprime_file <- tempfile()
  slendr::msp$sim_ancestry(N)$dump(msprime_file)
  msprime_file
}

# Make sure the unsimplified tree sequence has the same set of nodes as
# what we extracted with ts_data (sampled and all)
compare_ts_data <- function(ts, N) {
  data <- ts_data(ts)

  expect_true(nrow(data) == ts$num_nodes)
  expect_true(nrow(data[data$sampled, ]) == ts$num_samples)
  expect_true(nrow(data[data$sampled, ]) == 2 * N)

  ts2 <- ts_simplify(ts)
  data2 <- ts_data(ts2)

  # make sure the same holds also for a simplified tree sequence
  expect_true(nrow(data2) == ts2$num_nodes)
  expect_true(sum(data2$sampled) == ts2$num_samples)
  expect_true(sum(data2$sampled) == 2 * N)
}

# Make sure the extracted phylo tree structure is the same as what is encoded
# in the edge table
compare_ts_phylo <- function(ts, N) {
  ts2 <- ts_simplify(ts)

  tskit_tree <- ts2$at(0)
  tree <- ts_phylo(ts2, i = 1, quiet = TRUE)
  tree_data <- ts_data(tree)
  # make sure that the converted phylo tree has the same number of edges and
  # nodes as the original tskit Tree object
  expect_true(nrow(tree$edge) == tskit_tree$num_edges)
  expect_true((tree$Nnode + length(tree$tip.label)) == length(reticulate::iterate(tskit_tree$nodes(tskit_tree$root))))
  # the root must be the same node as well
  phylo_root <- length(tree$tip.label) + 1
  tskit_root <- tskit_tree$root
  expect_true(which(tree_data$node_id == tskit_root) == which(tree_data$phylo_id == phylo_root))

  expect_true(length(tree$tip.label) == nrow(tree_data[tree_data$sampled, ]))
}

test_that("non-slendr SLiM ts_data corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_slim_ts(N)
  ts <- ts_load(ts_file)
  compare_ts_data(ts, N)
})

test_that("non-slendr SLiM simplified ts_data corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_slim_ts(N)
  ts <- ts_load(ts_file)
  simplify_to <- ts_data(ts) %>% dplyr::filter(sampled) %>% dplyr::pull(node_id) %>% sample(3)
  ts2 <- ts_simplify(ts, simplify_to = simplify_to)

  # make sure that the simplified nodes match the pedigree_id values in the
  # original tree-sequence data
  orig_ids <- ts_data(ts) %>% dplyr::filter(node_id %in% simplify_to) %>% dplyr::pull(pedigree_id)
  simplified_ids <- ts_data(ts2) %>% dplyr::filter(sampled) %>% dplyr::pull(pedigree_id)
  expect_true(all(orig_ids == simplified_ids))
})

test_that("non-slendr SLiM ts_phylo corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_slim_ts(N)
  ts <- ts_load(ts_file)
  compare_ts_phylo(ts, N)
})

# msprime tree sequences --------------------------------------------------

test_that("non-slendr msprime ts_data corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_msprime_ts(N)
  ts <- ts_load(ts_file)
  expect_warning(compare_ts_data(ts, N), "If you want to simplify")
})

test_that("non-slendr SLiM simplified ts_data corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_msprime_ts(N)
  ts <- ts_load(ts_file)
  simplify_to <- ts_data(ts) %>% dplyr::filter(sampled) %>% dplyr::pull(node_id) %>% sample(3)
  ts2 <- ts_simplify(ts, simplify_to = simplify_to)

  # there are no fixed pedigree_id values, so let's check that the number of
  # "sampled" nodes corresponds to the number of nodes used for simplification
  expect_true(sum(ts_data(ts2)$sampled) == 3)
})

test_that("non-slendr msprime ts_phylo corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_msprime_ts(N)
  ts <- ts_load(ts_file)
  expect_warning(compare_ts_phylo(ts, N), "If you want to simplify")
})

test_that("non-slendr msprime ts_phylo corresponds to the expected outcome", {
  N <- 5
  ts_file <- simulate_msprime_ts(N)
  ts <- ts_load(ts_file)
  simplify_to <- ts_data(ts) %>% dplyr::filter(sampled) %>% dplyr::pull(node_id) %>% sample(3)
  ts2 <- ts_simplify(ts, simplify_to = simplify_to)
  expect_warning(compare_ts_phylo(ts2, N), "If you want to simplify")
})
