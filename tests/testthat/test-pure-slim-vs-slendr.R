# This unit test script makes sure that a trivially simple slendr model gives exactly the same
# result (i.e. tree sequence tables) after loading than a pure SLiM script

skip_if(!slendr:::check_env_present())
setup_env(quiet = TRUE)

# total length of the test simulation run
T <- 1000
# number of individuals in a populations
N <- 20

# run a slendr simulation -------------------------------------------------

pop <- population("pop", time = 1, N = N)
model <- compile_model(pop, generation_time = 1, direction = "forward", sim_length = T)
slim(model, sequence_length = 1, recombination_rate = 0, random_seed = 42)

# run a pure SLiM version of the same model -------------------------------

simulate_slim_ts <- function(N, T, output, script_file) {
  script_file <- tempfile()
  output <- tempfile()

  writeLines(sprintf('initialize() {
    setSeed(42);
    initializeSLiMOptions(keepPedigrees = T);
    initializeTreeSeq(retainCoalescentOnly=T);
    initializeMutationType(0, 0.5, "f", 0);
    initializeGenomicElementType(1, m0, 1);
    initializeGenomicElement(g1, 0, 0);
    initializeMutationRate(0);
    initializeRecombinationRate(0);
  }
  1 late() {
  	sim.addSubpop("p0", %d);
  }
  %s late() {
    inds = sample(sim.subpopulations.individuals, %s);
    sim.treeSeqRememberIndividuals(inds, permanent = T);
  }
  1: late() {
    sim.treeSeqRememberIndividuals(sim.subpopulations.individuals, permanent = F);
  }
  %s late() {
  	sim.treeSeqOutput("%s");
    catn(sim.generation + "finished");
  }
  ', N, T + 1, N, T + 1, output), script_file)
  out <- system2("slim", script_file, stdout = TRUE)
  ts_load(output)
}

# load tree sequences, extract tables -------------------------------------

ts1 <- ts_load(model)
ts2 <- simulate_slim_ts(N, T)

shared_cols <- c("node_id", "time_tskit", "sampled", "remembered", "retained", "alive", "pedigree_id", "pop_id", "ind_id")

table1 <- ts_nodes(ts1) %>% dplyr::arrange(time_tskit) %>% .[, shared_cols] %>% as.data.frame()
table2 <- ts_nodes(ts2) %>% dplyr::arrange(time_tskit) %>% .[, shared_cols] %>% as.data.frame()

test_that("pure SLiM and slendr versions of the same model give the same node/ind table", {
  expect_true(all(table1 == table2))
})

test_that("pure SLiM and slendr versions of the same model give the same phylo object", {
  t1 <- ts_simplify(ts1) %>% ts_phylo(1, quiet = TRUE)
  t2 <- ts_simplify(ts2) %>% ts_phylo(1, quiet = TRUE)

  expect_equal(t1$edge, t2$edge)
  expect_equal(t1$edge.length, t2$edge.length)
  expect_equal(t1$node.label, t2$node.label)
  expect_equal(t1$Nnode, t2$Nnode)
})
