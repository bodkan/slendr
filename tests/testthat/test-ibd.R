skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

test_that("aggregate ts_ibd(ts, coordinates = TRUE) matches IBD totals", {
  pop <- population("POP", time = 1, N = 1000)
  model <- compile_model(populations = pop, generation_time = 1, simulation_length = 1000)

  ts <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8)

  ibd_totals <- ts_ibd(ts, coordinates = FALSE, minimum_length = 500000)
  ibd_fragments <- ts_ibd(ts, coordinates = TRUE, minimum_length = 500000)

  # compute IBD totals from individual fragments manually
  ibd_totals2 <-
    dplyr::group_by(ibd_fragments, node1, node2, name1, name2, pop1, pop2, node1_time, node2_time) %>%
    dplyr::summarise(count = dplyr::n(), total = sum(length), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::select(node1, node2, count, total, node1_time, node2_time,
                  dplyr::everything()); ibd_totals2

  expect_equal(ibd_totals, ibd_totals2)
})

set.seed(42)

pop1 <- population("POP1", time = 1, N = 1000)
pop2 <- population("POP2", time = 1000, parent = pop1, N = 1000)
model <- compile_model(populations = list(pop1, pop2),
                        generation_time = 1, simulation_length = 2000)

ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8)
samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")

pop1_samples <- sample(samples$POP1, 20)
pop2_samples <- sample(samples$POP2, 20)

test_that("`within =` argument of ts_ibd() does what it is supposed to", {
  ibd_totals1 <- ts_ibd(ts, coordinates = FALSE, minimum_length = 1e6, within = pop1_samples)
  ibd_totals2 <- ts_ibd(ts, coordinates = FALSE, minimum_length = 1e6, within = pop2_samples)

  expect_true("POP1" == unique(unlist(ibd_totals1[, c("pop1", "pop2")])))
  expect_true("POP2" == unique(unlist(ibd_totals2[, c("pop1", "pop2")])))
})

test_that("`between =` argument of ts_ibd() does what it is supposed to", {
  # individual names in a named list
  ibd_totals1 <- ts_ibd(ts, coordinates = FALSE, minimum_length = 100000,
                        between = list(x = pop1_samples, y = pop2_samples))
  expect_true("POP1" == unique(unlist(ibd_totals1$pop1)))
  expect_true("POP2" == unique(unlist(ibd_totals1$pop2)))

  # individual names in an unnamed list
  ibd_totals2 <- ts_ibd(ts, coordinates = FALSE, minimum_length = 100000,
                        between = list(pop1_samples, pop2_samples))
  expect_true("POP1" == unique(unlist(ibd_totals2$pop1)))
  expect_true("POP2" == unique(unlist(ibd_totals2$pop2)))

  expect_true(all(ibd_totals1 == ibd_totals2))
})

test_that("IBD of a given minimum length is returned", {
  pop <- population("POP", time = 1, N = 1000)
  model <- compile_model(populations = pop, generation_time = 1, simulation_length = 1000)
  ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8, random_seed = 42)

  set.seed(42)
  samples <- sample(ts_samples(ts)$name, 10)

  ibd_totals <- ts_ibd(ts, coordinates = TRUE, within = samples, minimum_length = 6e6)
  expect_true(min(ibd_totals$length) >= 6e6)

  ibd_totals <- ts_ibd(ts, coordinates = TRUE, within = samples, minimum_length = 1e6)
  expect_true(min(ibd_totals$length) >= 1e6)

  ibd_totals <- ts_ibd(ts, coordinates = TRUE, within = samples, minimum_length = 100e3)
  expect_true(min(ibd_totals$length) >= 100e3)

  # ts_ibd() gives a warning when unrestricted IBD is requested by the user
  expect_warning(ibd_totals <- ts_ibd(ts, coordinates = TRUE, within = samples),
                 "No minimum IBD length (.*) or maximum age (.*)")
  expect_true(min(ibd_totals$length) >= 1)
})

# Get a vector of TMRCA of pairs of nodes that share IBD fragment
# (this is used below for testing that the `maximum_time` cutoff of
# the ts_ibd() function does what its supposed to).
#
# NOTE: This is actually not needed anymore. When I first wrote this
# I didn't realize that TreeSequence.ibd_segments() also returns a
# node ID of a MRCA of an IBD pair and, via TreeSequence.node(<MRCA ID>).time
# I can get the TMRCA itself.
#
# Still, I think this is a useful test of a good invariant, so I'll keep it here.
get_pairs_tmrca <- function(ts, ibd) {
  times <- c()
  # iterate over every detected IBD pair of nodes...
  for (ibd_i in seq_len(nrow(ibd))) {
    nodes <- as.integer(ibd[ibd_i, c("node1", "node2")])
    coords <- as.integer(ibd[ibd_i, c("left", "right")])
    # ... then search for the age of the MRCA of those nodes in trees within that IBD
    # segment
    for (tree_i in seq_len(ts$num_trees)) {
      tree <- ts_tree(ts, tree_i, mode = "index")
      # skip trees which are outside of the IBD segment's boundaries
      if (tree$interval$right <= coords[1] || tree$interval$left >= coords[2])
        next
      # record the time of the MRCA of the two nodes in the trees which fall within the IBD segment
      times <- c(times, tree$tmrca(nodes[1], nodes[2]))
    }
  }
  times
}

test_that("only IBD with MRCA of a given maximum age is reported", {
  pop <- population("POP", time = 1, N = 1000)
  model <- compile_model(populations = pop, generation_time = 1, simulation_length = 1000)
  ts <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42)

  set.seed(42)
  samples <- sample(ts_samples(ts)$name, 10)

  ibd_totals10 <- ts_ibd(ts, coordinates = TRUE, within = samples, maximum_time = 10)
  tmrca10 <- get_pairs_tmrca(ts, ibd_totals10)
  expect_true(max(tmrca10) <= 10)
  expect_equal(sort(unique(tmrca10)), sort(unique(ibd_totals10$tmrca)))

  ibd_totals20 <- ts_ibd(ts, coordinates = TRUE, within = samples, maximum_time = 20)
  tmrca20 <- get_pairs_tmrca(ts, ibd_totals20)
  expect_true(max(tmrca20) <= 20)
  expect_equal(sort(unique(tmrca20)), sort(unique(ibd_totals20$tmrca)))

  ibd_totals50 <- ts_ibd(ts, coordinates = TRUE, within = samples, maximum_time = 50)
  tmrca50 <- get_pairs_tmrca(ts, ibd_totals50)
  expect_true(max(tmrca50) <= 50)
  expect_equal(sort(unique(tmrca50)), sort(unique(ibd_totals50$tmrca)))

  ibd_totals100 <- ts_ibd(ts, coordinates = TRUE, within = samples, maximum_time = 100)
  tmrca100 <- get_pairs_tmrca(ts, ibd_totals100)
  expect_true(max(tmrca100) <= 100)
  expect_equal(sort(unique(tmrca100)), sort(unique(ibd_totals100$tmrca)))

  ibd_totals500 <- ts_ibd(ts, coordinates = TRUE, within = samples, maximum_time = 500)
  tmrca500 <- get_pairs_tmrca(ts, ibd_totals500)
  expect_true(max(tmrca100) <= 500)
  expect_equal(sort(unique(tmrca500)), sort(unique(ibd_totals500$tmrca)))
})
