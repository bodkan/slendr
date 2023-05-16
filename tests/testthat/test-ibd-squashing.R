skip_if(!is_slendr_env_present())

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

test_that("full tree sequence with unary nodes produces correct squashing results", {
  # get IBDs from a tree sequence without and with squashing
  ibd_full <- ts_ibd(ts_full, coordinates = TRUE, minimum_length = 0)
  ibd_full_squashed <- ts_ibd(ts_full, coordinates = TRUE, squash = TRUE, minimum_length = 0)

  # on the non-squashed IBD data frame, perform the squashing "manually"
  ibd_full_summary <- manual_squash(ibd_full)

  # the result must be equivalent to the squashing result done on the Python/tskit level
  expect_true(all(ibd_full_summary == ibd_full_squashed))
})


test_that("simplified tree sequence with/without unary nodes squashes IBDs correctly", {
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
  ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, minimum_length = 0)
  ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, minimum_length = 0)

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

test_that("simplified (subsetted) tree sequence with/without unary nodes squashes IBDs correctly", {
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
  ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, minimum_length = 0)
  ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, minimum_length = 0)

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

test_that("simplified tree sequence with/without unary nodes squashes `between` IBDs correctly", {
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
  ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, minimum_length = 0, between = list(0, 2))
  ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, minimum_length = 0, between = list(0, 2))

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

test_that("simplified tree sequence with/without unary nodes squashes `within` IBDs correctly", {
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
  ibd_simple_squashed <- ts_ibd(ts_simple, coordinates = TRUE, squash = TRUE, minimum_length = 0, within = list(0, 2))
  ibd_simple_unfilt_squashed <- ts_ibd(ts_simple_unfilt, coordinates = TRUE, squash = TRUE, minimum_length = 0, within = list(0, 2))

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
