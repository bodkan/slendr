slim_script <- tempfile()
slim_file <- tempfile()

writeLines(paste0('initialize() {
	initializeTreeSeq();
	initializeMutationRate(0);
	initializeMutationType("m1", 0.5, "f", 0.0);
	initializeGenomicElementType("g1", m1, 1.0);
	initializeGenomicElement(g1, 0, 1e8-1);
	initializeRecombinationRate(1e-8);
}
1 {
	sim.addSubpop("p1", 500);
}
5000 late() {
	sim.treeSeqOutput("', slim_file, '");
}
'), slim_script)

system2("slim", slim_script)
ts <- ts_load(slim_file)

msprime_file <- tempfile()
slendr::msp$sim_ancestry(5)$dump(msprime_file)
ts <- ts_load(msprime_file)

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
