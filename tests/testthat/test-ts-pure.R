skip_if(TRUE)

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
	sim.addSubpop("p1", 20);
}
4010 late() {
	sim.treeSeqOutput("', slim_file, '");
}
'), slim_script)

system2("slim", slim_script)
ts <- ts_load(slim_file)
x <- ts_data(ts)
ts2 <- ts_simplify(ts)
x2 <- ts_data(ts2)
tree <- ts_phylo(ts2, i = 1)
phytools::plotTree(tree)

msprime_file <- tempfile()
slendr::msp$sim_ancestry(5)$dump(msprime_file)
ts <- ts_load(msprime_file)
x <- ts_data(ts)
tree <- ts_phylo(ts, i = 1)
phytools::plotTree(tree)

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
