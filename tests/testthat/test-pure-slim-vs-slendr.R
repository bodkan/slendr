# This unit test script makes sure that a trivially simple slendr model gives exactly the same
# result (i.e. tree sequence tables) after loading than a pure SLiM script

# total length of the test simulation run
T <- 100
# number of individuals in a populations
N <- 10

# run a slendr simulation -------------------------------------------------

pop <- population("pop", time = 1, N = N)
model <- compile_model(pop, generation_time = 1, direction = "forward", sim_length = T)
samples <- schedule_sampling(model, times = T + 1, list(pop, N))
slim(model, sequence_length = 1, recombination_rate = 0, sampling = samples, random_seed = 42)

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

# t1 <- pyslim$load(path.expand(file.path(model$path, "output_slim.trees")))
# t2 <- pyslim$load("/Users/mp/Desktop/test.trees")
#
# x1 <- as.data.frame(get_ts_raw_nodes(t1))
# x2 <- as.data.frame(get_ts_raw_nodes(t2))
#
# print(x1)
# print(x2)

# all(x2$time == x3$time)
# all(x2$node_id == x3$node_id)
# all(x2$ind_id == x3$ind_id, na.rm = T)
#
ts1 <- ts_load(model)
ts2 <- simulate_slim_ts(N, T)

n1 <- ts_nodes(ts1) %>% dplyr::arrange(ind_id) %>% dplyr::select(-name, -pop) %>% as.data.frame()
n2 <- ts_nodes(ts2) %>% dplyr::arrange(ind_id) %>% dplyr::select(-pop) %>% as.data.frame()

n1
n2
