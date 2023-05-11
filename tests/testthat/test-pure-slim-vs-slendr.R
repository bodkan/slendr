# This unit test script makes sure that a trivially simple slendr model gives exactly the same
# result (i.e. tree sequence tables) after loading than a pure SLiM script

skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

# total length of the test simulation run
T <- 10000
# number of individuals in a populations
N <- 2000

# run a slendr simulation -------------------------------------------------

pop <- population("pop", time = 1, N = N)
model <- compile_model(pop, generation_time = 1, direction = "forward", simulation_length = T)

ts1 <- slim(model, method = "batch", sequence_length = 1,
            recombination_rate = 0, random_seed = 42, verbose = FALSE)

# run a pure SLiM version of the same model -------------------------------

simulate_slim_ts <- function(N, T, output, script_file , verbose = FALSE) {
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
    catn(community.tick + "finished");
  }
  2: fitnessEffect() /* Compute fitness of individuals */ {
    return 1.0;
    interaction = community.allInteractionTypes[2 * subpop.id]; // this must be here otherwise the test breaks
  }
  ', N, T + 1, N, T + 1, output), script_file)

  out <- system2("slim", script_file, stdout = TRUE)
  if (verbose) cat(paste(out, collapse = "\n"))

  ts_load(output)
}

ts2 <- simulate_slim_ts(N, T, verbose = FALSE)

# load tree sequences, extract tables -------------------------------------

shared_cols <- c("node_id", "time_tskit", "remembered", "retained", "alive", "pedigree_id", "pop_id", "ind_id")

table1 <- ts_nodes(ts1) %>% dplyr::arrange(pedigree_id) %>% .[, shared_cols] %>% as.data.frame()
table2 <- ts_nodes(ts2) %>% dplyr::arrange(pedigree_id) %>% .[, shared_cols] %>% as.data.frame()

test_that("pure SLiM and slendr versions of the same model give the same node/ind table", {
  expect_true(all(table1 == table2))
})

test_that("pure SLiM and slendr versions of the same model give the same phylo object", {
  t1 <- ts_recapitate(ts1, Ne = N, recombination_rate = 0, random_seed = 42) %>% ts_simplify() %>% ts_phylo(0, quiet = TRUE)
  t2 <- ts_recapitate(ts1, Ne = N, recombination_rate = 0, random_seed = 42) %>% ts_simplify() %>% ts_phylo(0, quiet = TRUE)

  # plot(t1)
  # plot(t2)

  expect_equal(t1$edge, t2$edge)
  expect_equal(t1$edge.length, t2$edge.length)
  expect_equal(t1$node.label, t2$node.label)
  expect_equal(t1$Nnode, t2$Nnode)
})

# simplification tests (after introducing constant tracking of names of sampled individuals)
test_that("simplification on pure SLiM tree sequence retains the correct data", {
  tmp_small <- tempfile()
  suppressWarnings(ts_small <- ts_simplify(ts2, simplify_to = c(0, 42, 100, 256)))
  ts_save(ts_small, tmp_small)
  ts_small_loaded <- ts_load(tmp_small)
  expect_equal(ts_nodes(ts_small_loaded) %>% dplyr::filter(sampled) %>% nrow, 4)

  # for a mysterious reason not worth investigating right now, the last two
  # columns of ts_nodes (ind_id, pop_id) are flipped between ts_small and ts_small_loaded,
  # so let's compare the ts_nodes contents by explicitly ordered columns
  cols <- c("pop", "node_id", "time", "time_tskit", "sampled", "remembered",
            "retained", "alive", "pedigree_id", "ind_id", "pop_id")
  expect_equal(ts_nodes(ts_small)[, cols], ts_nodes(ts_small_loaded)[, cols])
})
