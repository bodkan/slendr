skip_if(!slendr:::check_env_present())

set.seed(42)

script_file <- tempfile()
ts_file <- tempfile()

writeLines(sprintf('
initialize() {
  initializeSLiMOptions(dimensionality="xy");
  initializeTreeSeq();
  initializeMutationRate(0);
  initializeMutationType("m1", 0.5, "f", 0.0);
  initializeGenomicElementType("g1", m1, 1.0);
  initializeGenomicElement(g1, 0, 1e6);
  initializeRecombinationRate(1e-8);
}
1 late() {
  sim.addSubpop("p1", 500);
  p1.individuals.x = runif(p1.individualCount);
  p1.individuals.y = runif(p1.individualCount);
}
modifyChild() {
  // draw a child position near the first parent, within bounds
  do child.x = parent1.x + rnorm(1, 0, 0.02);
  while ((child.x < 0.0) | (child.x > 1.0));

  do child.y = parent1.y + rnorm(1, 0, 0.02);
  while ((child.y < 0.0) | (child.y > 1.0));

  return T;
}
2000 late() { sim.treeSeqOutput("%s"); }
', ts_file), script_file)

system2("slim", script_file, stdout = FALSE)

ts <- ts_load(ts_file)
