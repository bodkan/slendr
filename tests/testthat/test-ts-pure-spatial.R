skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

set.seed(42)

script_file <- tempfile()
ts_file <- tempfile()
loc_file <- tempfile()

writeLines(sprintf('
initialize() {
  initializeSLiMOptions(keepPedigrees = T, dimensionality = "xy");
  initializeTreeSeq();
  initializeMutationRate(0);
  initializeMutationType("m1", 0.5, "f", 0.0);
  initializeGenomicElementType("g1", m1, 1.0);
  initializeGenomicElement(g1, 0, 1e6);
  initializeRecombinationRate(1e-8);
}
1 late() {
  sim.addSubpop("p1", 100);
  p1.individuals.x = runif(p1.individualCount);
  p1.individuals.y = runif(p1.individualCount);
}
modifyChild() {
  do child.x = parent1.x + rnorm(1, 0, 0.02);
  while ((child.x < 0.0) | (child.x > 1.0));

  do child.y = parent1.y + rnorm(1, 0, 0.02);
  while ((child.y < 0.0) | (child.y > 1.0));

  return T;
}
5000 late() {
  sim.treeSeqOutput("%s");
  for (ind in sim.subpopulations.individuals) {
    writeFile("%s", paste(ind.spatialPosition, ind.pedigreeID, sep = "\t"), append = T);
  }
}
', ts_file, loc_file), script_file)

system2("slim", script_file, stdout = FALSE)

suppressMessages(ts <- ts_load(ts_file, simplify = TRUE))

test_that("non-slendr SLiM tree sequence locations are correctly loaded", {
  data <- ts_nodes(ts, sf = FALSE) %>%
    dplyr::arrange(pedigree_id) %>%
    dplyr::select(x, y, pedigree_id) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(pedigree_id)) %>%
    as.data.frame()
  locations <- readr::read_tsv(
    loc_file, col_types = "ddi",
    col_names = c("x", "y", "pedigree_id")
  ) %>% dplyr::arrange(pedigree_id)

  expect_true(all.equal(data$x, locations$x, tolerance = 0.00001))
  expect_true(all.equal(data$y, locations$y, tolerance = 0.00001))
  expect_true(all(data$pedigree_id == locations$pedigree_id))
})

test_that("ts_ibd() on spatial SLiM tree sequences works with coordinates = (T|F)", {
  suppressWarnings(ibd_totals <- ts_ibd(ts, coordinates = FALSE, sf = FALSE))
  suppressWarnings(ibd_fragments <- ts_ibd(ts, coordinates = TRUE, sf = FALSE))

  # compute IBD totals from individual fragments manually
  ibd_totals2 <-
    dplyr::group_by(ibd_fragments, node1, node2, node1_time, node2_time) %>%
    dplyr::summarise(count = dplyr::n(), total = sum(length), .groups = "keep") %>%
    dplyr::select(count, total, dplyr::everything()) %>%
    dplyr::select(node1, node2, count, total, node1_time, node2_time) %>%
    dplyr::ungroup()

  expect_equal(ibd_totals, ibd_totals2)
})

test_that("ts_ibd() on spatial SLiM tree sequences gives a correct sf object", {
  ibd_sf <- ts_ibd(ts, coordinates = FALSE, minimum_length = 1e6)
  ibd_nosf <- ts_ibd(ts, coordinates = FALSE, minimum_length = 1e6, sf = FALSE)

  # returned object is of a sf class (or not), as requested by the user
  expect_s3_class(ibd_sf, "sf")
  expect_true(!inherits(ibd_nosf, "sf"))

  # except for the spatial columns, the IBD results are the same
  expect_equal(as.data.frame(ibd_sf)[, c("node1", "node2", "count", "total",
                                         "node1_time", "node2_time")],
               as.data.frame(ibd_nosf))
})
