env_present <- function(path) { tryCatch(
  {
    reticulate::use_virtualenv(path, required = TRUE)
    return(TRUE)
  },
  error = function(cond) FALSE
)
}

skip_if(!env_present("~/.pyenv/versions/retipy"))

map <- world(xrange = c(0, 3500), yrange = c(0, 700), landscape = "blank")

N <- 100; y <- 350; r = 240
p1 <- population("pop1", time = 1, N = N, map = map, center = c(750, y), radius = r)
p2 <- population("pop2", parent = p1, time = 2, N = N, map = map, center = c(1750, y), radius = r)

model <- compile(
  populations = list(p1, p2),
  generation_time = 1, resolution = 1, sim_length = 300,
  competition_dist = 10, mate_dist = 10, dispersal_dist = 5,
  dir = file.path(tempdir(), "spatial-interactions"), overwrite = TRUE
)

samples <- rbind(
  sampling(model, times = 2, list(p1, 2), list(p2, 2)),
  sampling(model, times = 300, list(p1, 10), list(p2, 10))
)

slim(model, seq_length = 1, recomb_rate = 0, save_locations = TRUE, burnin = 10,
     ts_recording = TRUE, method = "batch", seed = 314159,
     sampling = samples, overwrite = TRUE, verbose = FALSE)

test_that("ts_load generates an object of the correct type", {
  ts <- ts_load(model, simplify = TRUE)
  expect_true(inherits(ts, "pyslim.slim_tree_sequence.SlimTreeSequence"))
  expect_true(inherits(ts, "tskit.trees.TreeSequence"))
})

test_that("tree sequence contains the specified number of sampled individuals", {
  ts <- ts_load(model, simplify = TRUE)
  counts <- ts_data(ts, remembered = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(ind_id, time, pop) %>%
    dplyr::count(time, pop)
  expect_true(all(counts == samples))
})

test_that("locations and times in the tree sequence match values saved by SLiM", {
  ts <- ts_load(model, simplify = TRUE)
  individuals <- ts_data(ts, remembered = TRUE) %>% dplyr::distinct(ind_id, .keep_all = TRUE)
  true_locations <- readr::read_tsv(file.path(model$path, "output_ind_locations.tsv.gz"),
                                    col_types = "iicidd")
  joined <- dplyr::inner_join(individuals, true_locations,
                              by = c("pedigree_id" = "ind")) %>%
    dplyr::mutate(location_x = as.vector(sf::st_coordinates(location)[, 1]),
                  location_y = as.vector(sf::st_coordinates(location)[, 2]))
  # for some reason the values differ in terms of decimal digits saved?
  # but they *are* equal
  expect_true(all.equal(joined$x, joined$location_x, tolerance = 1e-5))
  expect_true(all.equal(joined$y, joined$location_y, tolerance = 1e-5))
  expect_true(all.equal(joined$time.x, joined$time.y))
})

test_that("extracted individual, node and edge counts match the tree sequence", {
  ts1 <- ts_load(model)
  table1 <- ts_data(ts1)

  ts2 <- ts_load(model, recapitate = TRUE, Ne = 1000, recombination_rate = 0)
  table2 <- ts_data(ts2)

  ts3 <- ts_load(model, recapitate = TRUE, simplify = TRUE, Ne = 1000, recombination_rate = 0)
  table3 <- ts_data(ts3)

  expect_true(ts1$num_individuals == sum(!is.na(unique(table1$ind_id))))
  expect_true(ts2$num_individuals == sum(!is.na(unique(table2$ind_id))))
  expect_true(ts3$num_individuals == sum(!is.na(unique(table3$ind_id))))

  expect_true(ts1$num_nodes == nrow(table1))
  expect_true(ts2$num_nodes == nrow(table2))
  expect_true(ts3$num_nodes == nrow(table3))

  expect_true(all(sort(table1$node_id) == seq(0, ts1$num_nodes - 1)))
  expect_true(all(sort(table2$node_id) == seq(0, ts2$num_nodes - 1)))
  expect_true(all(sort(table3$node_id) == seq(0, ts3$num_nodes - 1)))

  expect_true(all(sort(unique(table1$ind_id)) == seq(0, ts1$num_individuals - 1)))
  expect_true(all(sort(unique(table2$ind_id)) == seq(0, ts2$num_individuals - 1)))
  expect_true(all(sort(unique(table3$ind_id)) == seq(0, ts3$num_individuals - 1)))

  expect_true(ts1$num_edges == nrow(ts_edges(ts1)))
  expect_true(ts2$num_edges == nrow(ts_edges(ts2)))
  expect_true(ts3$num_edges == nrow(ts_edges(ts3)))
})

test_that("simplification works only for present samples", {
  msg <- "The following individuals are not present"
  expect_error(ts_load(model, simplify = TRUE, simplify_to = "xyz"), msg)
})

test_that("simplification retains only specified samples", {
  ts <- ts_load(model, simplify = TRUE, simplify_to = c("pop11", "pop12"))
  expect_true(all(na.omit(unique(ts_data(ts)$name)) == c("pop11", "pop12")))

  ts2 <- ts_load(model)
  simplify_to <- sample(ts_samples(ts2)$name, 10)

  ts2 <- ts_simplify(ts2, simplify_to = simplify_to)
  expect_true(length(intersect(na.omit(unique(ts_data(ts2)$name)), simplify_to)) == length(simplify_to))
})
