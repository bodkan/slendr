skip_if(!dir.exists("~/.pyenv/versions/retipy"))

reticulate::use_virtualenv("~/.pyenv/versions/retipy", required = TRUE)

map <- world(xrange = c(0, 3500), yrange = c(0, 700), landscape = "blank")

N <- 100; y <- 350; r = 240
p1 <- population("pop1", time = 1, N = N, map = map, center = c(750, y), radius = r)
p2 <- population("pop2", time = 1, N = N, map = map, center = c(1750, y), radius = r)

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
  individuals <- ts_individuals(ts, remembered = TRUE) %>%
    dplyr::group_by(time, pop) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  expect_equal(individuals, samples)
})

test_that("locations and times in the tree sequence match values saved by SLiM", {
  ts <- ts_load(model, simplify = TRUE)
  individuals <- ts_individuals(ts, remembered = TRUE)
  true_locations <- readr::read_tsv(file.path(model$path, "output_ind_locations.tsv.gz"),
                                    col_types = "iicidd")
  joined <- dplyr::inner_join(individuals, true_locations, by = c("pedigree_id" = "ind"))
  # for some reason the values differ in terms of decimal digits saved?
  # but they *are* equal
  expect_true(all.equal(joined$raster_x, joined$x, tolerance = 1e-5))
  expect_true(all.equal(joined$time.x, joined$time.y))
})
