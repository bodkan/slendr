skip_if(!is_slendr_env_present())

test_that("minimal tree sequence (nodes+edges) is correctly loaded", {
  reticulate::py_run_file("manual_ts_nodes+edges.py")

  path <- reticulate::py$filename
  ts <- ts_load(path)

  # ts_tree(ts, 0) %>% ts_draw()

  edges <- ts_table(ts, "edges")
  expect_true(all(edges$child == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(edges$parent == c(2, 2, 5, 5, 6, 6)))

  expect_true(all(ts_table(ts, "nodes")$node_id == seq(0, 6)))
  expect_true(all(ts_table(ts, "nodes")$time_tskit == c(0, 0, 3, 0, 0, 7, 10)))

  expect_true(all(is.na(ts_table(ts, "nodes")$pop_id)))
  expect_true(all(is.na(ts_nodes(ts)$pop_id)))

  # check the annotated nodes table
  expect_true(all(ts_nodes(ts)$node_id == seq(0, 6)))
  expect_true(all(ts_nodes(ts)$time_tskit == c(0, 0, 3, 0, 0, 7, 10)))

  # check the annotated edges table
  expect_true(all(ts_edges(ts)$child_node_id == c(0, 1, 2, 3, 4, 5)))
  expect_true(all(ts_edges(ts)$parent_node_id == c(2, 2, 6, 5, 5, 6)))
})

test_that("minimal tree sequence (nodes+edges+inds) is correctly loaded", {
  reticulate::py_run_file("manual_ts_nodes+edges+inds.py")

  path <- reticulate::py$filename
  ts <- ts_load(path)

  # ts_tree(ts, 0) %>% ts_draw()

  edges <- ts_table(ts, "edges")
  expect_true(all(edges$child == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(edges$parent == c(2, 2, 5, 5, 6, 6)))

  expect_true(all(ts_table(ts, "nodes")$node_id == seq(0, 6)))
  expect_true(all(ts_table(ts, "nodes")$time_tskit == c(0, 0, 3, 0, 0, 7, 10)))

  expect_true(all(is.na(ts_table(ts, "nodes")$pop_id)))
  expect_true(all(is.na(ts_nodes(ts)$pop_id)))

  # check the annotated nodes table
  expect_true(all(ts_nodes(ts)$node_id == c(0, 1, 3, 4, 2, 5, 6)))
  expect_true(all(ts_nodes(ts)$time_tskit == c(0, 0, 0, 0, 3, 7, 10)))

  # check the annotated edges table
  expect_true(all(ts_edges(ts)$child_node_id == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(ts_edges(ts)$parent_node_id == c(2, 2, 5, 5, 6, 6)))
})

test_that("minimal tree sequence (nodes+edges+inds+pops) is correctly loaded", {
  reticulate::py_run_file("manual_ts_nodes+edges+inds+pops.py")

  path <- reticulate::py$filename
  ts <- ts_load(path)

  # ts_tree(ts, 0) %>% ts_draw()

  edges <- ts_table(ts, "edges")
  expect_true(all(edges$child == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(edges$parent == c(2, 2, 5, 5, 6, 6)))

  expect_true(all(ts_table(ts, "nodes")$node_id == seq(0, 6)))
  expect_true(all(ts_table(ts, "nodes")$time_tskit == c(0, 0, 3, 0, 0, 7, 10)))

  # check the annotated nodes table
  expect_true(all(ts_nodes(ts)$node_id == c(0, 1, 3, 4, 2, 5, 6)))
  expect_true(all(ts_nodes(ts)$time_tskit == c(0, 0, 0, 0, 3, 7, 10)))
  expect_true(all(ts_nodes(ts)$pop_id == c(0, 0, 2, 2, 1, 2, 1)))

  # check the annotated edges table
  expect_true(all(ts_edges(ts)$child_node_id == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(ts_edges(ts)$parent_node_id == c(2, 2, 5, 5, 6, 6)))
  expect_true(all(ts_edges(ts)$child_pop == c(0, 0, 2, 2, 1, 2)))
  expect_true(all(ts_edges(ts)$parent_pop == c(1, 1, 2, 2, 1, 1)))
})

test_that("minimal tree sequence (nodes+edges+inds+pops+muts) is correctly loaded", {
  reticulate::py_run_file("manual_ts_nodes+edges+inds+pops+muts.py")

  path <- reticulate::py$filename
  ts <- ts_load(path)

  # ts_tree(ts, 0) %>% ts_draw()

  edges <- ts_table(ts, "edges")
  expect_true(all(edges$child == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(edges$parent == c(2, 2, 5, 5, 6, 6)))

  expect_true(all(ts_table(ts, "nodes")$node_id == seq(0, 6)))
  expect_true(all(ts_table(ts, "nodes")$time_tskit == c(0, 0, 3, 0, 0, 7, 10)))

  expect_true(all(ts_table(ts, "mutations")$node == c(2, 5)))

  # check the annotated nodes table
  expect_true(all(ts_nodes(ts)$node_id == c(0, 1, 3, 4, 2, 5, 6)))
  expect_true(all(ts_nodes(ts)$time_tskit == c(0, 0, 0, 0, 3, 7, 10)))
  expect_true(all(ts_nodes(ts)$pop_id == c(0, 0, 2, 2, 1, 2, 1)))

  # check the annotated edges table
  expect_true(all(ts_edges(ts)$child_node_id == c(0, 1, 3, 4, 2, 5)))
  expect_true(all(ts_edges(ts)$parent_node_id == c(2, 2, 5, 5, 6, 6)))
  expect_true(all(ts_edges(ts)$child_pop == c(0, 0, 2, 2, 1, 2)))
  expect_true(all(ts_edges(ts)$parent_pop == c(1, 1, 2, 2, 1, 1)))
})
