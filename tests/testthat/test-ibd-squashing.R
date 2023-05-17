skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

reticulate::py_run_string("import warnings; import msprime; warnings.simplefilter('ignore', msprime.TimeUnitsMismatchWarning)")

#
# first batch of tests involves a small, manually constructed tree sequence below
#

io <- reticulate::import("io")
tskit <- reticulate::import("tskit")

nodes <- io$StringIO("id is_sample time
0 1 0
1 1 0
2 1 0
3 0 1
4 0 2
5 0 3
6 0 0.8
7 0 0.3
8 0 0.7
9 0 0.2"
)
edges <- io$StringIO("left right parent child
2 8 3 0
8 9 9 0
8 9 3 9
2 9 3 2
9 10 8 0
9 10 8 2
0 8 4 1
8 9 4 6
9 10 4 7
8 9 6 1
9 10 7 1
0 2 4 2
2 8 4 3
8 9 4 3
9 10 4 8
0 2 5 0
0 2 5 4"
)
tmp <- tempfile()
tskit$load_text(nodes = nodes, edges = edges, strict = FALSE)$dump(tmp)

manual_squash <- function(df) {
  df %>%
  dplyr::mutate(
    change_node1 = node1 != dplyr::lag(node1, default = node1[1]),
    change_node2 = node2 != dplyr::lag(node2, default = node2[1]),
    change_mrca = mrca != dplyr::lag(mrca, default = mrca[1]),
    group_id = cumsum(change_node1 | change_node2 | change_mrca)
  ) %>%
  dplyr::group_by(group_id) %>%
  dplyr::mutate(
    left = min(left),
    right = max(right),
    length = sum(length),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-group_id, -dplyr::starts_with("change_")) %>%
  dplyr::distinct()
}

ts_full <- ts_load(tmp)

# ts_draw(ts_full)

test_that("full tree sequence with unary nodes produces correct squashing results (manual)", {
  # get IBDs from a tree sequence without and with squashing
  ibd_full <- ts_ibd(ts_full, coordinates = TRUE, minimum_length = 0)
  suppressWarnings(ibd_full_squashed <- ts_ibd(ts_full, coordinates = TRUE, squash = TRUE))

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_full_summary <- manual_squash(ibd_full)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_full_summary == ibd_full_squashed))
})

test_that("simplified tree sequence with/without unary nodes squashes IBDs correctly (manual)", {
  # normal simplified t.s.
  ts_simple <- ts_simplify(ts_full)
  # t.s. simplified while keeping node indices intact
  ts_simple_unfilt <- ts_simplify(ts_full, filter_nodes = FALSE)

  # ts_draw(ts_simple)
  # ts_draw(ts_simple_unfilt)

  # find IBD segments on simplified tree sequences with or without unary nodes:
  # - non-squashed (default)
  ibd_simple <- ts_ibd(ts_simple, coordinates = TRUE, minimum_length = 0)
  ibd_simple_unfilt <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, minimum_length = 0)
  # - squashed
  suppressWarnings(ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE))
  suppressWarnings(ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE))

  # first, IBD from filtered and unfiltered simplified tree sequences must be the same
  # up to node numbering
  expect_true(all(
    dplyr::select(ibd_simple, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt, -node1, -node2, -mrca)
  ))

  # manual squashing using dplyr functions must be equivalent to the tskit squashing
  # on the Python level
  ibd_simple_summary <- manual_squash(ibd_simple)
  ibd_simple_unfilt_summary <- manual_squash(ibd_simple_unfilt)

  expect_true(all(ibd_simple_summary == ibd_simple_squashed))
  expect_true(all(ibd_simple_unfilt_summary == ibd_simple_unfilt_squashed))

  # a sanity check to make sure that the manual squashing summary done here is correct itself
  expect_true(all(
    dplyr::select(ibd_simple_summary, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt_summary, -node1, -node2, -mrca)
  ))
})

test_that("simplified (subsetted) tree sequence with/without unary nodes squashes IBDs correctly (manual)", {
  # normal simplified t.s.
  ts_simple <- ts_simplify(ts_full, simplify_to = c(0, 2))
  # t.s. simplified while keeping node indices intact
  ts_simple_unfilt <- ts_simplify(ts_full, filter_nodes = FALSE, simplify_to = c(0, 2))

  # ts_draw(ts_simple)
  # ts_draw(ts_simple_unfilt)

  # find IBD segments on simplified tree sequences with or without unary nodes:
  # - non-squashed (default)
  ibd_simple <- ts_ibd(ts_simple, coordinates = TRUE, minimum_length = 0)
  ibd_simple_unfilt <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, minimum_length = 0)
  # - squashed
  suppressWarnings(ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, minimum_length = 0))
  suppressWarnings(ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, minimum_length = 0))

  # first, IBD from filtered and unfiltered simplified tree sequences must be the same
  # up to node numbering
  expect_true(all(
    dplyr::select(ibd_simple, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt, -node1, -node2, -mrca)
  ))

  # manual squashing using dplyr functions must be equivalent to the tskit squashing
  # on the Python level
  ibd_simple_summary <- manual_squash(ibd_simple)
  ibd_simple_unfilt_summary <- manual_squash(ibd_simple_unfilt)

  expect_true(all(ibd_simple_summary == ibd_simple_squashed))
  expect_true(all(ibd_simple_unfilt_summary == ibd_simple_unfilt_squashed))

  # a sanity check to make sure that the manual squashing summary done here is correct itself
  expect_true(all(
    dplyr::select(ibd_simple_summary, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt_summary, -node1, -node2, -mrca)
  ))
})

test_that("simplified tree sequence with/without unary nodes squashes `between` IBDs correctly (manual)", {
  # normal simplified t.s.
  ts_simple <- ts_simplify(ts_full)
  # t.s. simplified while keeping node indices intact
  ts_simple_unfilt <- ts_simplify(ts_full, filter_nodes = FALSE)

  # ts_draw(ts_simple)
  # ts_draw(ts_simple_unfilt)

  # find IBD segments on simplified tree sequences with or without unary nodes:
  # - non-squashed (default)
  ibd_simple <- ts_ibd(ts_simple, coordinates = TRUE, minimum_length = 0, between = list(0, 2))
  ibd_simple_unfilt <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, minimum_length = 0, between = list(0, 2))
  # - squashed
  suppressWarnings(ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, between = list(0, 2)))
  suppressWarnings(ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, between = list(0, 2)))

  # first, IBD from filtered and unfiltered simplified tree sequences must be the same
  # up to node numbering
  expect_true(all(
    dplyr::select(ibd_simple, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt, -node1, -node2, -mrca)
  ))

  # manual squashing using dplyr functions must be equivalent to the tskit squashing
  # on the Python level
  ibd_simple_summary <- manual_squash(ibd_simple)
  ibd_simple_unfilt_summary <- manual_squash(ibd_simple_unfilt)

  expect_true(all(ibd_simple_summary == ibd_simple_squashed))
  expect_true(all(ibd_simple_unfilt_summary == ibd_simple_unfilt_squashed))

  # a sanity check to make sure that the manual squashing summary done here is correct itself
  expect_true(all(
    dplyr::select(ibd_simple_summary, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt_summary, -node1, -node2, -mrca)
  ))
})

test_that("simplified tree sequence with/without unary nodes squashes `within` IBDs correctly (manual)", {
  # normal simplified t.s.
  ts_simple <- ts_simplify(ts_full)
  # t.s. simplified while keeping node indices intact
  ts_simple_unfilt <- ts_simplify(ts_full, filter_nodes = FALSE)

  # ts_draw(ts_simple)
  # ts_draw(ts_simple_unfilt)

  # find IBD segments on simplified tree sequences with or without unary nodes:
  # - non-squashed (default)
  ibd_simple <- ts_ibd(ts_simple, coordinates = TRUE, minimum_length = 0, within = list(0, 2))
  ibd_simple_unfilt <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, minimum_length = 0, within = list(0, 2))
  # - squashed
  suppressWarnings(ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, within = list(0, 2)))
  suppressWarnings(ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, within = list(0, 2)))

  # first, IBD from filtered and unfiltered simplified tree sequences must be the same
  # up to node numbering
  expect_true(all(
    dplyr::select(ibd_simple, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt, -node1, -node2, -mrca)
  ))

  # manual squashing using dplyr functions must be equivalent to the tskit squashing
  # on the Python level
  ibd_simple_summary <- manual_squash(ibd_simple)
  ibd_simple_unfilt_summary <- manual_squash(ibd_simple_unfilt)

  expect_true(all(ibd_simple_summary == ibd_simple_squashed))
  expect_true(all(ibd_simple_unfilt_summary == ibd_simple_unfilt_squashed))

  # a sanity check to make sure that the manual squashing summary done here is correct itself
  expect_true(all(
    dplyr::select(ibd_simple_summary, -node1, -node2, -mrca) ==
    dplyr::select(ibd_simple_unfilt_summary, -node1, -node2, -mrca)
  ))
})

test_that("`squash = TRUE` with `minimum_length` cutoff gives a warning", {
  expect_warning(ts_ibd(ts_full, squash = TRUE, minimum_length = 1), "Please note that")
  expect_warning(ts_ibd(ts_full, squash = TRUE), "No minimum IBD")
})

#
# second batch of tests involves working with a slim tree sequence
#

test_that("tree sequence with unary nodes produces correct squashing results (SLiM raw)", {
  ts_slim <- population("pop", N = 10, time = 1) %>%
    compile_model(generation_time = 1, simulation_length = 1000) %>%
    slim(sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42, coalescent_only = FALSE)

  # get IBDs from a tree sequence without and with squashing
  ibd <- ts_ibd(ts_slim, coordinates = TRUE, minimum_length = 0)
  suppressWarnings(ibd_squashed <- ts_ibd(ts_slim, coordinates = TRUE, squash = TRUE))

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_summary <- manual_squash(ibd)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_summary == ibd_squashed))
})


test_that("tree sequence with unary nodes produces correct squashing results (SLiM recapitated)", {
  ts_slim <- population("pop", N = 10, time = 1) %>%
    compile_model(generation_time = 1, simulation_length = 1000) %>%
    slim(sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42, coalescent_only = FALSE) %>%
    ts_recapitate(Ne = 1000, recombination_rate = 1e-8, random_seed = 42)

  # get IBDs from a tree sequence without and with squashing
  ibd <- ts_ibd(ts_slim, coordinates = TRUE, minimum_length = 0)
  suppressWarnings(ibd_squashed <- ts_ibd(ts_slim, coordinates = TRUE, squash = TRUE))

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_summary <- manual_squash(ibd)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_summary == ibd_squashed))
})

test_that("tree sequence with unary nodes produces correct squashing results (SLiM unary simplified)", {
  ts_slim <- population("pop", N = 1000, time = 1) %>%
    compile_model(generation_time = 1, simulation_length = 1000) %>%
    slim(sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42, coalescent_only = FALSE) %>%
    ts_recapitate(Ne = 1000, recombination_rate = 1e-8, random_seed = 42) %>%
    ts_simplify(paste0("pop_", 1:10), filter_nodes = FALSE, keep_unary = TRUE)

  # get IBDs from a tree sequence without and with squashing
  ibd <- ts_ibd(ts_slim, coordinates = TRUE, minimum_length = 0)
  suppressWarnings(ibd_squashed <- ts_ibd(ts_slim, coordinates = TRUE, squash = TRUE))

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_summary <- manual_squash(ibd)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_summary == ibd_squashed))
})

test_that("tree sequence with unary nodes produces correct squashing results (SLiM no-unary simplified)", {
  ts_slim <- population("pop", N = 1000, time = 1) %>%
    compile_model(generation_time = 1, simulation_length = 1000) %>%
    slim(sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42, coalescent_only = FALSE) %>%
    ts_recapitate(Ne = 1000, recombination_rate = 1e-8, random_seed = 42) %>%
    ts_simplify(paste0("pop_", 1:10))

  # get IBDs from a tree sequence without and with squashing
  ibd <- ts_ibd(ts_slim, coordinates = TRUE, minimum_length = 0)
  suppressWarnings(ibd_squashed <- ts_ibd(ts_slim, coordinates = TRUE, squash = TRUE))

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_summary <- manual_squash(ibd)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_summary == ibd_squashed))
})

#
# third batch of tests involves working with a slim tree sequence
# TODO: check tests also with `filter_nodes = FALSE` (but see a bug below in ts_simplify())
#

ts_msprime <- population("pop", N = 1000, time = 1) %>%
  compile_model(generation_time = 1, simulation_length = 1000) %>%
  msprime(sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42)

# TODO: fix filter_nodes = FALSE for msprime tree sequences (branch `fix-msprime-filter-nodes`)
# ts_msprime %>% ts_simplify(simplify_to = paste0("pop_", 1:5), filter_nodes = FALSE)
ts_msprime <- ts_msprime %>% ts_simplify(simplify_to = paste0("pop_", 1:5))

test_that("full tree sequence with unary nodes produces correct squashing results (SLiM)", {
  # get IBDs from a tree sequence without and with squashing
  ibd <- ts_ibd(ts_slim, coordinates = TRUE, minimum_length = 0)
  suppressWarnings(ibd_squashed <- ts_ibd(ts_slim, coordinates = TRUE, squash = TRUE))

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_summary <- manual_squash(ibd)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_summary == ibd_squashed))
})
