skip_if(!slendr:::check_env_present())

pop <- population("POP", time = 1, N = 100)

model <- compile_model(populations = pop, generation_time = 1, sim_length = 100)

slim(model, sequence_length = 1000000, 0, random_seed = 42)

ts <- ts_load(model, recapitate = TRUE, simplify = TRUE,
              Ne = 100, recombination_rate = 0,
              simplify_to = paste0("POP_", seq(1, 10)))

# library(ggtree)
# tree <- ts_phylo(ts, 1)
# labels <- ts_nodes(tree) %>% dplyr::select(node = phylo_id, tskit_id = node_id)
# ggtree(tree, branch.length="none") %<+% labels +
#   geom_label(aes(label = tskit_id))

test_that("reconstructed ancestral relationships match what is seen in a tree", {
  expect_true(all(ts_ancestors(ts, 0)$parent_id == c(26, 33, 34, 35, 37, 38)))
  expect_true(all(ts_ancestors(ts, 15)$parent_id == c(23, 24, 29, 31, 34, 35, 37, 38)))
  expect_true(all(ts_ancestors(ts, 37)$parent_id == 38))
  expect_true(all(ts_ancestors(ts, 34)$parent_id == c(35, 37, 38)))
})

test_that("reconstructed descendant relationships match what is seen in a tree", {
  root_id <- ts_nodes(ts) %>% .[.$time == min(.$time), ] %>% .$node_id
  expect_true(all(sort(ts_descendants(ts, root_id)$child_id) == sort(setdiff(ts_nodes(ts)$node_id, root_id))))
  expect_true(all(sort(ts_descendants(ts, 32)$child_id) == c(7, 12)))
  expect_true(all(sort(ts_descendants(ts, 26)$child_id) == c(0, 6, 11, 20)))
  expect_true(all(sort(ts_descendants(ts, 30)$child_id) == c(5, 13, 16, 18, 21, 25)))
})
