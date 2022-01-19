skip_if(!env_present("retipy"))

map <- world(xrange = c(0, 3500), yrange = c(0, 700), landscape = "blank")

N <- 100; y <- 350; r <- 240
p1 <- population("pop1", time = 1, N = N, map = map, center = c(750, y), radius = r)
p2 <- population("pop2", parent = p1, time = 2, N = N, map = map, center = c(1750, y), radius = r)

res <- 1
desc <- "Test model without CRS"

model <- compile(
  populations = list(p1, p2),
  generation_time = 1, resolution = res, sim_length = 300,
  competition_dist = 10, mate_dist = 10, dispersal_dist = 5,
  dir = file.path(tempdir(), "spatial-interactions"), overwrite = TRUE,
  description = desc
)

samples <- rbind(
  sampling(model, times = 2, list(p1, 2), list(p2, 2)),
  sampling(model, times = 300, list(p1, 10), list(p2, 10))
)

slim(model, sequence_length = 100000, recombination_rate = 0, save_locations = TRUE, burnin = 10,
     method = "batch", seed = 314159,
     sampling = samples, verbose = FALSE)

test_that("ts_load generates an object of the correct type", {
  ts <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE)
  expect_true(inherits(ts, "pyslim.slim_tree_sequence.SlimTreeSequence"))
  expect_true(inherits(ts, "tskit.trees.TreeSequence"))
})

test_that("ts_save and ts_load result in the same tree sequence", {
  ts1 <- ts_load(model)
  file <- tempfile()
  ts_save(ts1, file)
  ts2 <- ts_load(model, file = file)

  data1 <- ts_data(ts1); data2 <- ts_data(ts2)
  samples1 <- ts_samples(ts1); samples2 <- ts_samples(ts2)

  expect_equal(data1, data2)
  expect_equal(samples1, samples2)
})

test_that("tree sequence contains the specified number of sampled individuals", {
  ts <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE)
  counts <- ts_data(ts, remembered = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(ind_id, time, pop) %>%
    dplyr::count(time, pop)
  expect_true(all(counts == samples[, c("time", "pop", "n")]))
})

test_that("locations and times in the tree sequence match values saved by SLiM", {
  ts <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE)
  individuals <- ts_data(ts, remembered = TRUE) %>% dplyr::distinct(ind_id, .keep_all = TRUE)
  true_locations <- readr::read_tsv(file.path(model$path, "output_ind_locations.tsv.gz"),
                                    col_types = "iicidd") %>%
    dplyr::mutate(time = convert_slim_time(gen, model))
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
  expect_error(ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE, simplify_to = "xyz"), msg)
})

test_that("simplification retains only specified samples", {
  ts <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE,
                simplify_to = c("pop1_1", "pop1_2"))
  expect_true(all(stats::na.omit(unique(ts_data(ts)$name)) == c("pop1_1", "pop1_2")))

  ts2 <- ts_load(model, recapitate = TRUE, recombination_rate = 0, Ne = 1)
  simplify_to <- sample(ts_samples(ts2)$name, 10)

  ts2 <- ts_simplify(ts2, simplify_to = simplify_to)
  expect_true(length(intersect(stats::na.omit(unique(ts_data(ts2)$name)), simplify_to)) == length(simplify_to))
})

test_that("ts_samples() names match ts_data() information", {
  ts1 <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0)
  ts2 <- ts_load(model, recapitate = TRUE, Ne = 1000, recombination_rate = 0)
  ts3 <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0,
                 simplify = TRUE, simplify_to = c("pop1_1", "pop1_2"))
  simplify_to <- sample(ts_samples(ts1)$name, 10)
  ts4 <- ts_simplify(ts1, simplify_to = simplify_to)

  expect_equal(sort(unique(ts_data(ts1, remembered = TRUE)$name)), sort(ts_samples(ts1)$name))
  expect_equal(sort(unique(ts_data(ts2, remembered = TRUE)$name)), sort(ts_samples(ts2)$name))
  expect_equal(sort(unique(ts_data(ts3, remembered = TRUE)$name)), sort(ts_samples(ts3)$name))
  expect_equal(sort(unique(ts_data(ts4, remembered = TRUE)$name)), sort(ts_samples(ts4)$name))
})

test_that("ts_eigenstrat requires recapitated and mutated data", {
  ts1 <- ts_load(model)
  ts2 <- ts_load(model, recapitate = TRUE, Ne = 1000, recombination_rate = 0)
  ts3 <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE,
                 simplify_to = c("pop1_1", "pop1_2"))
  ts4 <- ts_load(model, simplify = TRUE, recapitate = TRUE, recombination_rate = 0, Ne = 10000)
  ts5 <- ts_mutate(ts4, mutation_rate = 1e-7)
  ts6 <- ts_load(model, simplify = TRUE, recapitate = TRUE, recombination_rate = 0, Ne = 10000,
                 mutate = TRUE, mutation_rate = 1e-7)

  prefix <- file.path(tempdir(), "eigen")
  expect_error(ts_eigenstrat(ts1, prefix), "Tree sequence was not recapitated")
  expect_error(ts_eigenstrat(ts2, prefix), "Attempting to extract genotypes")
  expect_silent(suppressMessages(ts_eigenstrat(ts6, prefix)))

  path <- file.path(tempdir(), "gt.vcf.gz")
  expect_error(ts_vcf(ts1, path), "Tree sequence was not recapitated")
  expect_warning(ts_vcf(ts2, path), "Attempting to extract genotypes")
  expect_silent(suppressMessages(ts_vcf(ts5, path)))
  expect_silent(suppressMessages(ts_vcf(ts6, path)))
})

test_that("ts_mutate cannot be called on an already mutated tree sequence", {
  ts <- ts_load(model)
  ts_mut <- ts_mutate(ts, mutation_rate = 1e-7)
  expect_error(ts_mutate(ts_mut, mutation_rate = 2e-7),
               "Tree sequence already mutated")
})

test_that("ts_simplify-ing a non-recapitated tree sequence gives a warning", {
  ts <- ts_load(model)
  expect_warning(ts_simplify(ts), "Simplifying a non-recapitated tree sequence.")
  expect_silent(ts_simplify(ts, keep_input_roots = TRUE))
})

test_that("mutation rate must be present in order to mutate a tree sequence", {
  expect_error(ts_load(model, mutate = TRUE), "Mutation rate must be given ")
})

test_that("ts_eigenstrat and tsv_cf create correct data", {
  ts <- ts_load(model, simplify = TRUE, recapitate = TRUE, recombination_rate = 0, Ne = 10000) %>%
    ts_mutate(mutation_rate = 1e-7)

  ts_names <- sort(unique(ts_data(ts, remembered = TRUE)$name))

  # match EIGENSTRAT contents
  prefix <- file.path(tempdir(), "eigen")
  eigenstrat <- suppressMessages(ts_eigenstrat(ts, prefix))
  ind_names <- sort(admixr::read_ind(eigenstrat)$id)
  expect_true(all(ind_names == ts_names))

  # match VCF contents
  path <- file.path(tempdir(), "gt.vcf.gz")
  ts_vcf(ts, path)
  file <- gzfile(path)
  vcf_names <- readLines(file) %>%
    grep("^#CHROM", ., value = TRUE) %>%
    strsplit("\t") %>% .[[1]] %>% .[10 : length(.)] %>% sort
  on.exit(close(file))
  expect_true(all(vcf_names == ts_names))
})

test_that("ts_eigenstrat correctly adds an outgroup when instructed", {
  ts <- ts_load(model, simplify = TRUE, recapitate = TRUE, recombination_rate = 0, Ne = 10000) %>%
    ts_mutate(mutation_rate = 1e-7)

  ts_names <- sort(unique(ts_data(ts, remembered = TRUE)$name))

  # match EIGENSTRAT contents
  prefix <- file.path(tempdir(), "eigen")
  eigenstrat <- suppressMessages(ts_eigenstrat(ts, prefix, outgroup = "outgroup_ind"))
  ind_names <- sort(admixr::read_ind(eigenstrat)$id)
  expect_true(all(ind_names == c("outgroup_ind", ts_names)))
})

test_that("slendr metadata is correctly loaded (spatial model without CRS)", {
  output <- paste0(tempfile(), "spatial_test")

  burnin_length <- 123
  max_attempts <- 3
  recomb_rate <- 0.001
  save_locations <- FALSE
  seed <- 987
  sequence_length <- 999

  slim(model, sequence_length = sequence_length, recombination_rate = recomb_rate,
       save_locations = save_locations, burnin = burnin_length,
       method = "batch", seed = seed, max_attempts = max_attempts,
       sampling = samples, verbose = FALSE, output = output)

  ts <- ts_load(model, file = paste0(output, "_slim.trees"))
  metadata <- ts_metadata(ts)

  expect_true(stringr::str_replace(metadata$version, "slendr_", "") == packageVersion("slendr"))
  expect_true(all(sf::st_bbox(map) == metadata$map$EXTENT))
  expect_true(metadata$map$resolution == res)
  expect_true(is.null(metadata$map$crs))
  expect_true(metadata$description == desc)

  args <- metadata$arguments
  expect_equal(args$BURNIN_LENGTH, burnin_length)
  expect_equal(args$MAX_ATTEMPTS, max_attempts)
  expect_equal(args$RECOMB_RATE, recomb_rate)
  expect_equal(args$SEED, seed)
  expect_equal(args$SEQUENCE_LENGTH, sequence_length)
})

test_that("slendr metadata is correctly loaded (non-spatial model)", {
  output <- paste0(tempfile(), "non-spatial_test")

  burnin_length <- 123
  recomb_rate <- 0.001
  save_locations <- FALSE
  seed <- 987
  sequence_length <- 999
  spatial <- FALSE

  slim(model, sequence_length = sequence_length, recombination_rate = recomb_rate,
       save_locations = save_locations, burnin = burnin_length,
       method = "batch", seed = seed,
       sampling = samples, verbose = FALSE, spatial = spatial, output = output)

  ts <- ts_load(model, file = paste0(output, "_slim.trees"))
  metadata <- ts_metadata(ts)

  expect_true(stringr::str_replace(metadata$version, "slendr_", "") == packageVersion("slendr"))
  expect_true(is.null(metadata$map))
  expect_true(metadata$description == desc)

  args <- metadata$arguments
  expect_equal(args$BURNIN_LENGTH, burnin_length)
  expect_equal(args$RECOMB_RATE, recomb_rate)
  expect_equal(args$SEED, seed)
  expect_equal(args$SEQUENCE_LENGTH, sequence_length)
})

test_that("ts_mutate and mutation through ts_load give the same result", {
  ts <- ts_load(model, simplify = TRUE, recapitate = TRUE, seed = 123,
                recombination_rate = 0, Ne = 100)
  ts_mut1 <- ts_mutate(ts, mutation_rate = 1e-7, seed = 123)
  ts_mut2 <- ts_load(model, simplify = TRUE, recapitate = TRUE, mutate = TRUE, mutation_rate = 1e-7,
                     seed = 123, recombination_rate = 0, Ne = 100)
  expect_equal(suppressMessages(ts_genotypes(ts_mut1)),
               suppressMessages(ts_genotypes(ts_mut2)))
})

test_that("ts_mutate correctly specifies the SLiM mutation type", {
  ts <- ts_load(model, simplify = TRUE, recapitate = TRUE, seed = 123,
                recombination_rate = 0, Ne = 100)
  ts_mut1 <- ts_mutate(ts, mutation_rate = 1e-7, seed = 123)
  ts_mut2 <- ts_mutate(ts, mutation_rate = 1e-7, seed = 123, mut_type = 123456789)

  get_mut_type <- function(m) {
    mut_metadata <- m$metadata$mutation_list
    if (length(mut_metadata) > 0)
      return(mut_metadata[[1]]$mutation_type)
    else
      return(NULL)
  }

  mut_types1 <- unlist(reticulate::iterate(ts_mut1$mutations(), get_mut_type))
  mut_types2 <- unlist(reticulate::iterate(ts_mut2$mutations(), get_mut_type))

  expect_true(is.null(mut_types1))
  expect_true(all(mut_types2 == 123456789))
})

test_that("tree sequence contains the specified number of sampled individuals (default sampling)", {
  slim(model, sequence_length = 100000, recombination_rate = 0, save_locations = TRUE, burnin = 10,
     method = "batch", seed = 314159,
     verbose = FALSE)

  suppressMessages(
    ts <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE)
  )
  counts <- ts_data(ts, remembered = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(ind_id, time, pop) %>%
    dplyr::count(time, pop)
  expect_true(all(counts$n == N))
})

test_that("locations and times in the tree sequence match values saved by SLiM (default sampling)", {
  slim(model, sequence_length = 100000, recombination_rate = 0, save_locations = TRUE, burnin = 10,
     method = "batch", seed = 314159,
     verbose = FALSE)

  suppressMessages(
    ts <- ts_load(model, recapitate = TRUE, Ne = 1, recombination_rate = 0, simplify = TRUE)
  )
  individuals <- ts_data(ts, remembered = TRUE) %>% dplyr::distinct(ind_id, .keep_all = TRUE)
  true_locations <- readr::read_tsv(file.path(model$path, "output_ind_locations.tsv.gz"),
                                    col_types = "iicidd") %>%
    dplyr::mutate(time = convert_slim_time(gen, model))
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
