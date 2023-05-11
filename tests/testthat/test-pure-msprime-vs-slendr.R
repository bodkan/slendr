# This unit test script makes sure that a trivially simple slendr model gives exactly the same
# result (i.e. tree sequence tables) after loading than a pure SLiM script

skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

# number of individuals in a populations
N <- 1000

# run a slendr simulation -------------------------------------------------

pop <- population("pop", time = 1, N = N)
model <- compile_model(pop, generation_time = 1, direction = "forward", simulation_length = 1000)
ts1 <- msprime(model, sequence_length = 1, recombination_rate = 0, random_seed = 42)

# run a pure SLiM version of the same model -------------------------------

simulate_msprime_ts <- function(N) {
  output <- tempfile()
  py_cmd <- sprintf("import msprime; msprime.sim_ancestry(%d, random_seed=42, population_size=%d).dump('%s')", N, N, output)
  reticulate::py_run_string(py_cmd)
  ts_load(output)
}

# load tree sequences, extract tables -------------------------------------

ts2 <- simulate_msprime_ts(N)

shared_cols <- c("node_id", "time_tskit", "sampled", "pop_id", "ind_id")

table1 <- ts_nodes(ts1) %>% dplyr::arrange(time_tskit) %>% .[, shared_cols] %>% as.data.frame()
table2 <- ts_nodes(ts2) %>% dplyr::arrange(time_tskit) %>% .[, shared_cols] %>% as.data.frame()

test_that("pure msprime and slendr versions of the same model give the same node/ind table", {
  expect_true(all(table1 == table2, na.rm = TRUE))
})

test_that("pure msprime and slendr versions of the same model give the same phylo object", {
  t1 <- ts1 %>% ts_phylo(0, quiet = TRUE)
  t2 <- ts2 %>% ts_phylo(0, quiet = TRUE)

  expect_equal(t1$edge, t2$edge)
  expect_equal(t1$edge.length, t2$edge.length)
  expect_equal(t1$node.label, t2$node.label)
  expect_equal(t1$Nnode, t2$Nnode)
})

# simplification tests (after introducing constant tracking of names of sampled individuals)
test_that("simplification on pure msprime tree sequence retains the correct data", {
  tmp_small <- tempfile()
  suppressWarnings(ts_small <- ts_simplify(ts2, simplify_to = c(0, 42, 100, 256)))
  ts_save(ts_small, tmp_small)
  ts_small_loaded <- ts_load(tmp_small)
  expect_equal(ts_nodes(ts_small_loaded) %>% dplyr::filter(sampled) %>% nrow, 4)

  expect_equal(ts_nodes(ts_small), ts_nodes(ts_small_loaded))
})
