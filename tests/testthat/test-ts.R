skip_if(!check_dependencies(python = TRUE))

init_env(quiet = TRUE)

map <- world(xrange = c(0, 3500), yrange = c(0, 700), landscape = "blank")

N <- 100; y <- 350; r <- 240
p1 <- population("pop1", time = 1, N = N, map = map, center = c(750, y), radius = r)
p2 <- population("pop2", parent = p1, time = 2, N = N, map = map, center = c(1750, y), radius = r)

res <- 1
desc <- "Test model without CRS"

model_dir <- file.path(tempdir(), "ts")

model <- compile_model(
  populations = list(p1, p2),
  generation_time = 1, resolution = res, simulation_length = 300,
  competition = 10, mating = 10, dispersal = 5,
  path = model_dir, overwrite = TRUE, force = TRUE,
  description = desc
)

samples <- rbind(
  schedule_sampling(model, times = 2, list(p1, 2), list(p2, 2)),
  schedule_sampling(model, times = 300, list(p1, 10), list(p2, 10))
)

slim_ts <- normalizePath(tempfile(fileext = ".trees"), winslash = "/", mustWork = FALSE)
msprime_ts <- normalizePath(tempfile(fileext = ".trees"), winslash = "/", mustWork = FALSE)

locations_file <- normalizePath(tempfile(fileext = ".gz"), winslash = "/", mustWork = FALSE)

slim(model, sequence_length = 100000, recombination_rate = 0,
     locations = locations_file, burnin = 0,
     method = "batch", random_seed = 314159,
     samples = samples, verbose = FALSE) %>% ts_write(slim_ts)

msprime(model, sequence_length = 100000, recombination_rate = 0,
        random_seed = 314159, samples = samples, verbose = FALSE) %>% ts_write(msprime_ts)

test_that("ts_read fails gracefully on incorrect filename", {
  expect_error(ts <- ts_read(model, file = paste0(msprime_ts, "blah")), "File not found")
})

test_that("ts_read generates an object of the correct type (SLiM)", {
  ts <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0) %>% ts_simplify()
  expect_true(inherits(ts, "tskit.trees.TreeSequence"))
})

test_that("unnecessary recapitation is prevented (msprime)", {
  expect_warning(
    ts_read(model, file = msprime_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0),
    "There is no need to recapitate"
  )
})

test_that("ts_read generates an object of the correct type (msprime)", {
  ts <- ts_read(model, file = msprime_ts)
  expect_true(inherits(ts, "tskit.trees.TreeSequence"))
})

test_that("ts_write and ts_read result in the same tree sequence (SLiM)", {
  ts1 <- ts_read(model, file = slim_ts)
  file <- paste0(tempfile(), "_slim.trees")
  ts_write(ts1, file)
  ts2 <- ts_read(model, file = file)

  data1 <- ts_nodes(ts1); data2 <- ts_nodes(ts2)
  samples1 <- ts_samples(ts1); samples2 <- ts_samples(ts2)

  expect_equal(data1, data2)
  expect_equal(samples1, samples2)
})

test_that("ts_write and ts_read result in the same tree sequence (msprime)", {
  ts1 <- ts_read(model, file = msprime_ts)
  file <- paste0(tempfile(), "_msprime.trees")
  ts_write(ts1, file)
  ts2 <- ts_read(model, file = file)

  data1 <- ts_nodes(ts1); data2 <- ts_nodes(ts2)
  samples1 <- ts_samples(ts1); samples2 <- ts_samples(ts2)

  expect_equal(data1, data2)
  expect_equal(samples1, samples2)
})

test_that("tree sequence contains the right number of sampled individuals (SLiM)", {
  ts <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0) %>%
    ts_simplify()
  counts <- ts_nodes(ts) %>%
    dplyr::filter(sampled) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(ind_id, time, pop) %>%
    dplyr::count(time, pop)
  expect_true(all(counts == samples[, c("time", "pop", "n")]))
})

test_that("locations and times in the tree sequence match values saved by SLiM", {
  ts <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0) %>% ts_simplify()
  individuals <- ts_nodes(ts) %>% dplyr::filter(sampled) %>% dplyr::distinct(ind_id, .keep_all = TRUE)
  true_locations <- readr::read_tsv(locations_file, col_types = "iicidd") %>%
    dplyr::mutate(time = convert_slim_time(gen, model))
  joined <- dplyr::inner_join(individuals, true_locations,
                              by = c("pedigree_id" = "ind")) %>%
    dplyr::mutate(location_x = as.vector(sf::st_coordinates(location)[, 1]),
                  location_y = as.vector(sf::st_coordinates(location)[, 2])) %>%
    as.data.frame()
  # for some reason the values differ in terms of decimal digits saved?
  # but they *are* equal
  expect_true(all.equal(joined$x, joined$location_x, tolerance = 1e-5))
  expect_true(all.equal(joined$y, joined$location_y, tolerance = 1e-5))
  expect_true(all.equal(joined$time.x, joined$time.y))
})

test_that("extracted individual, node, edge, and mutation counts match the tree sequence (SLiM)", {
  ts1 <- ts_read(model, file = slim_ts)
  table1 <- ts_nodes(ts1)

  ts2 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0)
  table2 <- ts_nodes(ts2)

  ts3 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0) %>% ts_simplify()
  table3 <- ts_nodes(ts3)

  suppressWarnings(ts4 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0) %>% ts_mutate(mutation_rate = 1e-6))
  table4 <- ts_nodes(ts4)

  expect_true(ts1$num_individuals == sum(!is.na(unique(table1$ind_id))))
  expect_true(ts2$num_individuals == sum(!is.na(unique(table2$ind_id))))
  expect_true(ts3$num_individuals == sum(!is.na(unique(table3$ind_id))))
  expect_true(ts4$num_individuals == sum(!is.na(unique(table4$ind_id))))

  expect_true(ts1$num_nodes == nrow(table1))
  expect_true(ts2$num_nodes == nrow(table2))
  expect_true(ts3$num_nodes == nrow(table3))
  expect_true(ts4$num_nodes == nrow(table4))

  expect_true(all(sort(table1$node_id) == seq(0, ts1$num_nodes - 1)))
  expect_true(all(sort(table2$node_id) == seq(0, ts2$num_nodes - 1)))
  expect_true(all(sort(table3$node_id) == seq(0, ts3$num_nodes - 1)))
  expect_true(all(sort(table4$node_id) == seq(0, ts4$num_nodes - 1)))

  expect_true(all(sort(unique(table1$ind_id)) == seq(0, ts1$num_individuals - 1)))
  expect_true(all(sort(unique(table2$ind_id)) == seq(0, ts2$num_individuals - 1)))
  expect_true(all(sort(unique(table3$ind_id)) == seq(0, ts3$num_individuals - 1)))
  expect_true(all(sort(unique(table4$ind_id)) == seq(0, ts4$num_individuals - 1)))

  expect_true(ts1$num_edges == nrow(ts_table(ts1, "edges")))
  expect_true(ts2$num_edges == nrow(ts_table(ts2, "edges")))
  expect_true(ts3$num_edges == nrow(ts_table(ts3, "edges")))
  expect_true(ts4$num_edges == nrow(ts_table(ts4, "edges")))

  expect_true(ts1$num_mutations == nrow(ts_table(ts1, "mutations")))
  expect_true(ts2$num_mutations == nrow(ts_table(ts2, "mutations")))
  expect_true(ts3$num_mutations == nrow(ts_table(ts3, "mutations")))
  expect_true(ts4$num_mutations == nrow(ts_table(ts4, "mutations")))
})

test_that("extracted individual, node, edge, and mutation counts match the tree sequence (msprime)", {
  # this is not a super useful test as no recapitation or simplification would
  # be performed -- but a good sanity check to enforce that ts1 == ts2 == ts3
  ts1 <- ts_read(model, file = msprime_ts)
  table1 <- ts_nodes(ts1)

  suppressWarnings(ts2 <- ts_read(model, file = msprime_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0))
  table2 <- ts_nodes(ts2)

  suppressWarnings(ts3 <- ts_read(model, file = msprime_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0) %>% ts_simplify())
  table3 <- ts_nodes(ts3)

  suppressWarnings(ts4 <- ts_read(model, file = msprime_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0) %>% ts_simplify() %>% ts_mutate(mutation_rate = 1e-6))
  table4 <- ts_nodes(ts4)

  expect_true(ts1$num_individuals == sum(!is.na(unique(table1$ind_id))))
  expect_true(ts2$num_individuals == sum(!is.na(unique(table2$ind_id))))
  expect_true(ts3$num_individuals == sum(!is.na(unique(table3$ind_id))))
  expect_true(ts4$num_individuals == sum(!is.na(unique(table4$ind_id))))

  expect_true(ts1$num_nodes == nrow(table1))
  expect_true(ts2$num_nodes == nrow(table2))
  expect_true(ts3$num_nodes == nrow(table3))
  expect_true(ts4$num_nodes == nrow(table4))

  expect_true(all(sort(table1$node_id) == seq(0, ts1$num_nodes - 1)))
  expect_true(all(sort(table2$node_id) == seq(0, ts2$num_nodes - 1)))
  expect_true(all(sort(table3$node_id) == seq(0, ts3$num_nodes - 1)))
  expect_true(all(sort(table4$node_id) == seq(0, ts4$num_nodes - 1)))

  expect_true(all(sort(unique(table1$ind_id)) == seq(0, ts1$num_individuals - 1)))
  expect_true(all(sort(unique(table2$ind_id)) == seq(0, ts2$num_individuals - 1)))
  expect_true(all(sort(unique(table3$ind_id)) == seq(0, ts3$num_individuals - 1)))
  expect_true(all(sort(unique(table4$ind_id)) == seq(0, ts4$num_individuals - 1)))

  expect_true(ts1$num_edges == nrow(ts_table(ts1, "edges")))
  expect_true(ts2$num_edges == nrow(ts_table(ts2, "edges")))
  expect_true(ts3$num_edges == nrow(ts_table(ts3, "edges")))
  expect_true(ts4$num_edges == nrow(ts_table(ts4, "edges")))

  expect_true(ts1$num_mutations == nrow(ts_table(ts1, "mutations")))
  expect_true(ts2$num_mutations == nrow(ts_table(ts2, "mutations")))
  expect_true(ts3$num_mutations == nrow(ts_table(ts3, "mutations")))
  expect_true(ts4$num_mutations == nrow(ts_table(ts4, "mutations")))

  expect_true(ts1 == ts2)

  # the following is no longer true because simplification must remove potential unary nodes,
  # at the very least
  # expect_true(ts1 == ts3)
})

test_that("simplification works only for samples that are really present", {
  msg <- "The following individuals are not present"
  expect_error(ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0) %>% ts_simplify(simplify_to = "xyz"), msg)
  expect_error(ts_read(model, file = msprime_ts) %>% ts_simplify(simplify_to = "xyz"), msg)
})

test_that("simplification retains only specified samples (SLiM)", {
  simplify_to <- c("pop1_1", "pop1_2", "pop2_7")
  ts <- ts_read(model, file = slim_ts) %>%
    ts_recapitate(Ne = 1, recombination_rate = 0) %>%
    ts_simplify(simplify_to = simplify_to)
  df_samples <- ts_samples(ts) %>% dplyr::arrange(name, time)
  df_data <- ts_nodes(ts) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time)
  expect_true(all(df_data$name == df_samples$name))
  expect_true(all(df_data$time == df_samples$time))
  expect_true(all(df_data$pop == df_samples$pop))

  suppressWarnings(ts2 <- ts_read(model, file = slim_ts) %>%
    ts_recapitate(recombination_rate = 0, Ne = 1))
  simplify_to <- sample(ts_samples(ts2)$name, 10)

  ts2 <- ts_simplify(ts2, simplify_to = simplify_to)
  df_samples2 <- ts_samples(ts2) %>% dplyr::arrange(name, time)
  df_data2 <- ts_nodes(ts2) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time)
  expect_true(all(df_data2$name == df_samples2$name))
  expect_true(all(df_data2$time == df_samples2$time))
  expect_true(all(df_data2$pop == df_samples2$pop))
})

test_that("simplification retains only specified samples (msprime)", {
  simplify_to <- c("pop1_1", "pop1_2", "pop2_7")
  ts <- ts_read(model, file = msprime_ts) %>% ts_simplify(simplify_to = simplify_to)
  df_samples <- ts_samples(ts) %>% dplyr::arrange(name, time)
  df_data <- ts_nodes(ts) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time)
  expect_true(all(df_data$name == df_samples$name))
  expect_true(all(df_data$time == df_samples$time))
  expect_true(all(df_data$pop == df_samples$pop))

  suppressWarnings(ts2 <- ts_read(model, file = msprime_ts) %>% ts_recapitate(recombination_rate = 0, Ne = 1))
  simplify_to <- sample(ts_samples(ts2)$name, 10)

  ts2 <- ts_simplify(ts2, simplify_to = simplify_to)
  df_samples2 <- ts_samples(ts2) %>% dplyr::arrange(name, time)
  df_data2 <- ts_nodes(ts2) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time)
  expect_true(all(df_data2$name == df_samples2$name))
  expect_true(all(df_data2$time == df_samples2$time))
  expect_true(all(df_data2$pop == df_samples2$pop))
})

test_that("ts_samples() names match ts_nodes() information (SLiM)", {
  simplify_to <- c("pop1_1", "pop1_2", "pop2_7")

  ts1 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0)
  ts2 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0)
  ts3 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0) %>%
    ts_simplify(simplify_to = simplify_to)
  simplify_to <- sample(ts_samples(ts1)$name, 10)
  ts4 <- ts_simplify(ts1, simplify_to = simplify_to)

  df_samples1 <- ts_samples(ts1) %>% dplyr::arrange(name, time)
  df_data1 <- ts_nodes(ts1) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data1$name == df_samples1$name))
  expect_true(all(df_data1$time == df_samples1$time))
  expect_true(all(df_data1$pop == df_samples1$pop))

  df_samples2 <- ts_samples(ts2) %>% dplyr::arrange(name, time)
  df_data2 <- ts_nodes(ts2) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data2$name == df_samples2$name))
  expect_true(all(df_data2$time == df_samples2$time))
  expect_true(all(df_data2$pop == df_samples2$pop))

  df_samples3 <- ts_samples(ts3) %>% dplyr::arrange(name, time)
  df_data3 <- ts_nodes(ts3) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data3$name == df_samples3$name))
  expect_true(all(df_data3$time == df_samples3$time))
  expect_true(all(df_data3$pop == df_samples3$pop))

  df_samples4 <- ts_samples(ts4) %>% dplyr::arrange(name, time)
  df_data4 <- ts_nodes(ts4) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data4$name == df_samples4$name))
  expect_true(all(df_data4$time == df_samples4$time))
  expect_true(all(df_data4$pop == df_samples4$pop))
})

test_that("ts_samples() names match ts_nodes() information (msprime)", {
  simplify_to <- c("pop1_1", "pop1_2", "pop2_7")

  ts1 <- ts_read(model, file = msprime_ts)
  ts3 <- ts_read(model, file = msprime_ts) %>% ts_simplify(simplify_to = simplify_to)
  simplify_to <- sample(ts_samples(ts1)$name, 10)
  ts4 <- ts_simplify(ts1, simplify_to = simplify_to)

  df_samples1 <- ts_samples(ts1) %>% dplyr::arrange(name, time)
  df_data1 <- ts_nodes(ts1) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data1$name == df_samples1$name))
  expect_true(all(df_data1$time == df_samples1$time))
  expect_true(all(df_data1$pop == df_samples1$pop))

  df_samples3 <- ts_samples(ts3) %>% dplyr::arrange(name, time)
  df_data3 <- ts_nodes(ts3) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data3$name == df_samples3$name))
  expect_true(all(df_data3$time == df_samples3$time))
  expect_true(all(df_data3$pop == df_samples3$pop))

  df_samples4 <- ts_samples(ts4) %>% dplyr::arrange(name, time)
  df_data4 <- ts_nodes(ts4) %>% stats::na.omit() %>% dplyr::distinct(name, .keep_all = TRUE) %>% dplyr::arrange(name, time) %>% as.data.frame()
  expect_true(all(df_data4$name == df_samples4$name))
  expect_true(all(df_data4$time == df_samples4$time))
  expect_true(all(df_data4$pop == df_samples4$pop))
})

test_that("ts_eigenstrat requires recapitated and mutated data (SLiM)", {
  skip_if(Sys.which("qpDstat") == "")
  ts1 <- ts_read(model, file = slim_ts)
  ts2 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1000, recombination_rate = 0)
  ts3 <- ts_read(model, file = slim_ts) %>% ts_recapitate(Ne = 1, recombination_rate = 0) %>% ts_simplify(simplify_to = c("pop1_1", "pop1_2"))
  ts4 <- ts_read(model, file = slim_ts) %>% ts_recapitate(recombination_rate = 0, Ne = 10000) %>%
    ts_simplify()
  ts5 <- ts_mutate(ts4, mutation_rate = 1e-7)
  ts6 <- ts_read(model, file = slim_ts) %>% ts_recapitate(recombination_rate = 0, Ne = 10000) %>%
    ts_simplify() %>%
    ts_mutate(mutation_rate = 1e-7)

  prefix <- file.path(tempdir(), "eigen")
  expect_error(ts_eigenstrat(ts1, prefix), "Tree sequence was not recapitated")
  expect_error(ts_eigenstrat(ts2, prefix), "Attempting to extract genotypes")
  expect_error(ts_eigenstrat(ts3, prefix), "Attempting to extract genotypes")
  expect_error(ts_eigenstrat(ts4, prefix), "Attempting to extract genotypes")
  suppressMessages(expect_s3_class(ts_eigenstrat(ts5, prefix), "EIGENSTRAT"))
  suppressMessages(expect_s3_class(ts_eigenstrat(ts6, prefix), "EIGENSTRAT"))

  path <- file.path(tempdir(), "gt.vcf.gz")
  expect_error(
    expect_warning(ts_vcf(ts1, path), "Attempting to extract genotypes"),
    "Tree sequence was not recapitated"
  )
  expect_silent(suppressMessages(ts_vcf(ts5, path)))
  expect_silent(suppressMessages(ts_vcf(ts6, path)))
})

test_that("ts_eigenstrat requires recapitated and mutated data (msprime)", {
  skip_if(Sys.which("qpDstat") == "")
  ts1 <- ts_read(model, file = msprime_ts)
  ts3 <- ts_read(model, file = msprime_ts) %>% ts_simplify(simplify_to = c("pop1_1", "pop1_2", "pop2_7"))
  ts4 <- ts_read(model, file = msprime_ts) %>% ts_simplify()
  ts5 <- ts_mutate(ts4, mutation_rate = 1e-7)
  ts6 <- ts_read(model, file = msprime_ts) %>% ts_simplify(simplify_to = c("pop1_1", "pop1_2", "pop2_7")) %>% ts_mutate(mutation_rate = 1e-7)

  # this is not supposed to fail (unlike the previous SLiM case) because all
  # msprime sequences are "recapitated" by virtue of beling coalescent!
  prefix <- file.path(tempdir(), "eigen")
  expect_error(ts_eigenstrat(ts1, prefix), "Attempting to extract genotypes")
  expect_error(ts_eigenstrat(ts3, prefix), "Attempting to extract genotypes")
  expect_error(ts_eigenstrat(ts4, prefix), "Attempting to extract genotypes")
  expect_s3_class(ts_eigenstrat(ts5, prefix), "EIGENSTRAT")
  expect_s3_class(ts_eigenstrat(ts6, prefix), "EIGENSTRAT")

  path <- file.path(tempdir(), "gt.vcf.gz")
  expect_silent(suppressMessages(ts_vcf(ts5, path)))
  expect_silent(suppressMessages(ts_vcf(ts6, path)))
})

test_that("ts_mutate cannot be called on an already mutated tree sequence (SLiM)", {
  ts <- ts_read(model, file = slim_ts)
  ts_mut <- ts_mutate(ts, mutation_rate = 1e-7)
  expect_error(ts_mutate(ts_mut, mutation_rate = 2e-7),
               "Tree sequence already mutated")
})

test_that("ts_mutate cannot be called on an already mutated tree sequence (msprime)", {
  ts <- ts_read(model, file = msprime_ts)
  ts_mut <- ts_mutate(ts, mutation_rate = 1e-7)
  expect_error(ts_mutate(ts_mut, mutation_rate = 2e-7),
               "Tree sequence already mutated")
})

test_that("ts_simplify-ing a non-recapitated tree sequence gives a warning", {
  ts <- ts_read(model, file = slim_ts)
  expect_warning(ts_simplify(ts), "Simplifying a non-recapitated tree sequence.")
  expect_silent(ts_simplify(ts, keep_input_roots = TRUE))
})

test_that("ts_eigenstrat and tsv_cf create correct data (SLiM)", {
  skip_if(Sys.which("qpDstat") == "")
  ts <- ts_read(model, file = slim_ts) %>% ts_recapitate(recombination_rate = 0, Ne = 10000) %>%
    ts_simplify() %>%
    ts_mutate(mutation_rate = 1e-7)

  ts_names <- sort(unique(ts_nodes(ts) %>% dplyr::filter(sampled) %>% .$name))

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

test_that("ts_eigenstrat and tsv_cf create correct data (msprime)", {
  skip_if(Sys.which("qpDstat") == "")
  ts <- ts_read(model, file = msprime_ts) %>% ts_mutate(mutation_rate = 1e-7)

  ts_names <- sort(unique(ts_nodes(ts) %>% .$name))

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

test_that("ts_eigenstrat correctly adds an outgroup when instructed (SLiM)", {
  skip_if(Sys.which("qpDstat") == "")
  ts <- ts_read(model, file = slim_ts) %>% ts_recapitate(recombination_rate = 0, Ne = 10000) %>%
    ts_simplify() %>%
    ts_mutate(mutation_rate = 1e-7)

  ts_names <- sort(unique(ts_nodes(ts) %>% dplyr::filter(sampled) %>% .$name))

  # match EIGENSTRAT contents
  prefix <- file.path(tempdir(), "eigen")
  eigenstrat <- suppressMessages(ts_eigenstrat(ts, prefix, outgroup = "outgroup_ind"))
  ind_names <- sort(admixr::read_ind(eigenstrat)$id)
  expect_true(all(ind_names == c("outgroup_ind", ts_names)))
})

test_that("ts_eigenstrat correctly adds an outgroup when instructed (msprime)", {
  skip_if(Sys.which("qpDstat") == "")
  ts <- ts_read(model, file = msprime_ts) %>% ts_mutate(mutation_rate = 1e-7)

  ts_names <- sort(unique(ts_nodes(ts) %>% .$name))

  # match EIGENSTRAT contents
  prefix <- file.path(tempdir(), "eigen")
  eigenstrat <- suppressMessages(ts_eigenstrat(ts, prefix, outgroup = "outgroup_ind"))
  ind_names <- sort(admixr::read_ind(eigenstrat)$id)
  expect_true(all(ind_names == c("outgroup_ind", ts_names)))
})

test_that("slendr metadata is correctly loaded (spatial model without CRS)", {
  output <- normalizePath(paste0(tempfile(), "spatial_test"), winslash = "/", mustWork = FALSE)

  burnin_length <- 123
  max_attempts <- 3
  RECOMBINATION_RATE <- 0.001
  locations_file <- normalizePath(tempfile(fileext = ".gz"), winslash = "/", mustWork = FALSE)
  seed <- 987
  sequence_length <- 999

  ts <- slim(model, sequence_length = sequence_length, recombination_rate = RECOMBINATION_RATE,
       locations = locations_file, burnin = burnin_length,
       method = "batch", random_seed = seed, max_attempts = max_attempts,
       samples = samples, verbose = FALSE)
  ts_write(ts, output)

  metadata <- ts_metadata(ts)

  expect_true(gsub("slendr_", "", metadata$version) == packageVersion("slendr"))
  expect_true(all(sf::st_bbox(map) == metadata$map$EXTENT))
  expect_true(metadata$map$resolution == res)
  expect_true(is.null(metadata$map$crs))
  expect_true(metadata$description == desc)

  args <- metadata$arguments
  expect_equal(args$BURNIN_LENGTH, burnin_length)
  expect_equal(args$MAX_ATTEMPTS, max_attempts)
  expect_equal(args$RECOMBINATION_RATE, RECOMBINATION_RATE)
  expect_equal(args$SEED, seed)
  expect_equal(args$SEQUENCE_LENGTH, sequence_length)
})

test_that("slendr metadata is correctly loaded (non-spatial SLiM model)", {
  output <- normalizePath(paste0(tempfile(), "non-spatial_SLiM_test"), winslash = "/", mustWork = FALSE)

  burnin_length <- 123
  RECOMBINATION_RATE <- 0.001
  locations_file <- normalizePath(tempfile(fileext = ".gz"), winslash = "/", mustWork = FALSE)
  seed <- 987
  sequence_length <- 999
  spatial <- FALSE

  ts <- slim(model, sequence_length = sequence_length, recombination_rate = RECOMBINATION_RATE,
       locations = locations_file, burnin = burnin_length,
       method = "batch", random_seed = seed,
       samples = samples, verbose = FALSE, spatial = spatial)
  ts_write(ts, output)

  metadata <- ts_metadata(ts)

  expect_true(gsub("slendr_", "", metadata$version) == packageVersion("slendr"))
  expect_true(is.null(metadata$map))
  expect_true(metadata$description == desc)

  args <- metadata$arguments
  expect_equal(args$BURNIN_LENGTH, burnin_length)
  expect_equal(args$RECOMBINATION_RATE, RECOMBINATION_RATE)
  expect_equal(args$SEED, seed)
  expect_equal(args$SEQUENCE_LENGTH, sequence_length)
})

test_that("slendr metadata is correctly loaded (non-spatial msprime model)", {
  output <- paste0(tempfile(), "non-spatial_msprime_test")

  burnin_length <- 123
  RECOMBINATION_RATE <- 0.001
  seed <- 987
  sequence_length <- 999
  spatial <- FALSE

  ts <- msprime(model, sequence_length = sequence_length, recombination_rate = RECOMBINATION_RATE,
       random_seed = seed, samples = samples, verbose = FALSE)
  ts_write(ts, output)
  metadata <- ts_metadata(ts)

  expect_true(gsub("slendr_", "", metadata$version) == packageVersion("slendr"))
  expect_true(is.null(metadata$map))
  expect_true(metadata$description == desc)

  args <- metadata$arguments
  expect_equal(args$RECOMBINATION_RATE, RECOMBINATION_RATE)
  expect_equal(args$SEED, seed)
  expect_equal(args$SEQUENCE_LENGTH, sequence_length)
})

test_that("ts_mutate and mutation through ts_read give the same result (SLiM)", {
  ts <- ts_read(model, file = slim_ts) %>%
    ts_recapitate(random_seed = 123, recombination_rate = 0, Ne = 100) %>% ts_simplify()
  ts_mut1 <- ts_mutate(ts, mutation_rate = 1e-7, random_seed = 123)
  ts_mut2 <- ts_read(model, file = slim_ts) %>% ts_recapitate(random_seed = 123, recombination_rate = 0, Ne = 100) %>% ts_simplify() %>% ts_mutate(mutation_rate = 1e-7, random_seed = 123)
  expect_equal(suppressMessages(ts_genotypes(ts_mut1)),
               suppressMessages(ts_genotypes(ts_mut2)))
})

test_that("ts_mutate and mutation through ts_read give the same result (msprime)", {
  ts <- ts_read(model, file = msprime_ts)
  ts_mut1 <- ts_mutate(ts, mutation_rate = 1e-7, random_seed = 123)
  ts_mut2 <- ts_read(msprime_ts, model) %>% ts_mutate(mutation_rate = 1e-7, random_seed = 123)
  expect_equal(suppressMessages(ts_genotypes(ts_mut1)),
               suppressMessages(ts_genotypes(ts_mut2)))
})

test_that("ts_mutate correctly specifies the SLiM mutation type", {
  ts <- ts_read(slim_ts, model) %>%
    ts_recapitate(random_seed = 123, recombination_rate = 0, Ne = 100) %>%
    ts_simplify()
  ts_mut1 <- ts_mutate(ts, mutation_rate = 1e-7, random_seed = 123)
  msprime_module <- reticulate::import("msprime")
  ts_mut2 <- ts_mutate(ts, mutation_rate = 1e-7, random_seed = 123,
                       mutation_model = msprime_module$SLiMMutationModel(123456789L))

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
  ts <- slim(model, sequence_length = 100000, recombination_rate = 0, locations = locations_file, burnin = 10,
       method = "batch", random_seed = 314159, verbose = FALSE) %>%
    ts_recapitate(Ne = 1, recombination_rate = 0) %>%
    ts_simplify()

  counts <- ts_nodes(ts) %>%
    dplyr::filter(sampled) %>%
    dplyr::as_tibble() %>%
    dplyr::distinct(ind_id, time, pop) %>%
    dplyr::count(time, pop)
  expect_true(all(counts$n == N))
})

test_that("locations and times in the tree sequence match values saved by SLiM (default sampling)", {
  ts <- slim(model, sequence_length = 100000, recombination_rate = 0, locations = locations_file, burnin = 10,
       method = "batch", random_seed = 314159, verbose = FALSE) %>%
    ts_recapitate(Ne = 1, recombination_rate = 0) %>%
    ts_simplify()

  individuals <- ts_nodes(ts) %>% dplyr::filter(sampled) %>% dplyr::distinct(ind_id, .keep_all = TRUE)
  true_locations <- readr::read_tsv(locations_file, col_types = "iicidd") %>%
    dplyr::mutate(time = convert_slim_time(gen, model))
  joined <- dplyr::inner_join(individuals, true_locations,
                              by = c("pedigree_id" = "ind")) %>%
    as.data.frame() %>%
    dplyr::mutate(location_x = as.vector(sf::st_coordinates(location)[, 1]),
                  location_y = as.vector(sf::st_coordinates(location)[, 2]))
  # for some reason the values differ in terms of decimal digits saved?
  # but they *are* equal
  expect_true(all.equal(joined$x, joined$location_x, tolerance = 1e-5))
  expect_true(all.equal(joined$y, joined$location_y, tolerance = 1e-5))
  expect_true(all.equal(joined$time.x, joined$time.y))
})

test_that("metadata is the same for SLiM and msprime conditional on a model", {
  samples <- rbind(
    schedule_sampling(model, times = 2, list(p1, 2), list(p2, 2)),
    schedule_sampling(model, times = 212, list(p1, 5), list(p2, 3)),
    schedule_sampling(model, times = 300, list(p1, 10), list(p2, 10))
  )

  slim_ts <- normalizePath(file.path(model_dir, "output_slim.trees"), winslash = "/", mustWork = FALSE)
  msprime_ts <- normalizePath(file.path(model_dir, "msprime_output.trees"), winslash = "/", mustWork = FALSE)

  slim(model, sequence_length = 100000, recombination_rate = 0,
       locations = locations_file, burnin = 10,
       method = "batch", random_seed = 314159,
       samples = samples, verbose = FALSE) %>% ts_write(slim_ts)

  msprime(model, sequence_length = 100000, recombination_rate = 0,
          random_seed = 314159, samples = samples, verbose = FALSE) %>% ts_write(msprime_ts)

  simplify_to <- c("pop1_1", "pop1_2", "pop1_17")

  sts1 <- ts_read(model = model, file = slim_ts)
  sts2 <- ts_read(model = model, file = slim_ts) %>%
    ts_recapitate(Ne = 1000, recombination_rate = 0)
  sts3 <- ts_read(model = model, file = slim_ts) %>%
    ts_recapitate(Ne = 1, recombination_rate = 0) %>%
    ts_simplify(simplify_to = simplify_to)
  sts4 <- ts_read(model = model, file = slim_ts) %>%
    ts_recapitate(recombination_rate = 0, Ne = 10000) %>%
    ts_simplify()
  sts5 <- ts_mutate(sts4, mutation_rate = 1e-7)
  sts6 <- ts_read(model = model, file = slim_ts) %>%
    ts_recapitate(recombination_rate = 0, Ne = 10000) %>%
    ts_simplify() %>%
    ts_mutate(mutation_rate = 1e-7)

  mts1 <- ts_read(model = model, file = msprime_ts)
  mts2 <- ts_read(model = model, file = msprime_ts)
  mts3 <- ts_read(model = model, file = msprime_ts) %>% ts_simplify(simplify_to = simplify_to)
  suppressWarnings(mts4 <- ts_read(model = model, file = msprime_ts) %>% ts_simplify())
  mts5 <- ts_mutate(mts4, mutation_rate = 1e-7)
  mts6 <- ts_read(model = model, file = msprime_ts) %>% ts_mutate(mutation_rate = 1e-7)

  expect_equal(ts_samples(sts1), ts_samples(mts1))
  expect_equal(ts_samples(sts2), ts_samples(mts2))
  expect_equal(ts_samples(sts3), ts_samples(mts3))
  expect_equal(ts_samples(sts4), ts_samples(mts4))
  expect_equal(ts_samples(sts5), ts_samples(mts5))
  expect_equal(ts_samples(sts6), ts_samples(mts6))

  sdata1 <- ts_nodes(sts1) %>% dplyr::filter(sampled) %>% dplyr::arrange(name) %>% as.data.frame()
  mdata1 <- ts_nodes(mts1) %>% stats::na.omit() %>% dplyr::arrange(name) %>% as.data.frame()
  sdata2 <- ts_nodes(sts2) %>% dplyr::filter(sampled) %>% dplyr::arrange(name) %>% as.data.frame()
  mdata2 <- ts_nodes(mts2) %>% stats::na.omit() %>% dplyr::arrange(name) %>% as.data.frame()
  sdata3 <- ts_nodes(sts3) %>% dplyr::filter(sampled) %>% dplyr::arrange(name) %>% as.data.frame()
  mdata3 <- ts_nodes(mts3) %>% stats::na.omit() %>% dplyr::arrange(name) %>% as.data.frame()
  sdata4 <- ts_nodes(sts4) %>% dplyr::filter(sampled) %>% dplyr::arrange(name) %>% as.data.frame()
  mdata4 <- ts_nodes(mts4) %>% stats::na.omit() %>% dplyr::arrange(name) %>% as.data.frame()
  sdata5 <- ts_nodes(sts5) %>% dplyr::filter(sampled) %>% dplyr::arrange(name) %>% as.data.frame()
  mdata5 <- ts_nodes(mts5) %>% stats::na.omit() %>% dplyr::arrange(name) %>% as.data.frame()
  sdata6 <- ts_nodes(sts6) %>% dplyr::filter(sampled) %>% dplyr::arrange(name) %>% as.data.frame()
  mdata6 <- ts_nodes(mts6) %>% stats::na.omit() %>% dplyr::arrange(name) %>% as.data.frame()

  expect_equal(sdata1$name, mdata1$name)
  expect_equal(sdata2$name, mdata2$name)
  expect_equal(sdata3$name, mdata3$name)
  expect_equal(sdata4$name, mdata4$name)
  expect_equal(sdata5$name, mdata5$name)
  expect_equal(sdata6$name, mdata6$name)

  expect_equal(sdata1$time, mdata1$time)
  expect_equal(sdata2$time, mdata2$time)
  expect_equal(sdata3$time, mdata3$time)
  expect_equal(sdata4$time, mdata4$time)
  expect_equal(sdata5$time, mdata5$time)
  expect_equal(sdata6$time, mdata6$time)
})

test_that("all names of individuals must be present in the tree sequence", {
  ts <- ts_read(file = msprime_ts, model = model)
  expect_error(ts_diversity(ts, c("p1_1", "p2_2")), "Not all individual names")
  expect_s3_class(ts_diversity(ts, c("pop1_1", "pop2_2")), "data.frame")
})

# --------------------------------------------------------------------------------
# IBD-related test for spatial tree sequences (more is in test-ibd.R,
# test-ts-pure-spatial.R and test-ts-pure-nonspatial.R)

test_that("ts_ibd() on spatial SLiM/slendr tree sequences works properly (no CRS)", {
  suppressWarnings(ts_slim <- ts_read(model, file = slim_ts) %>% ts_simplify())

  suppressWarnings(slim_ibd_sf <- ts_ibd(ts_slim, coordinates = TRUE))
  suppressWarnings(slim_ibd_nosf <- ts_ibd(ts_slim, coordinates = TRUE, sf = FALSE))

  # returned object is of a sf class (or not), as requested by the user
  expect_s3_class(slim_ibd_sf, "sf")
  expect_true(!inherits(slim_ibd_nosf, "sf"))

  # the model does not have a CRS, and neither should the IBD table
  expect_true(is.na(sf::st_crs(slim_ibd_sf)))

  # spatial IBD links within the same individual give an EMPTY linestring with
  # no coordinates
  within_ibd <- slim_ibd_sf %>% dplyr::filter(name1 == name2) %>% dplyr::select(connection)
  expect_true(nrow(sf::st_coordinates(within_ibd)) == 0)

  # except for the spatial columns, the IBD results are the same
  expect_true(all(dplyr::select(as.data.frame(slim_ibd_sf),
                            -node1_location, -node2_location, -connection) ==
               as.data.frame(slim_ibd_nosf)))
})

test_that("ts_ibd() on msprime/slendr tree sequences works properly", {
  ts_msprime <- ts_read(model, file = msprime_ts)

  suppressWarnings(msprime_ibd_sf <- ts_ibd(ts_msprime, coordinates = TRUE))
  suppressWarnings(msprime_ibd_nosf <- ts_ibd(ts_msprime, coordinates = TRUE, sf = FALSE))

  # returned object is of a sf class (or not), as requested by the user
  expect_true(!inherits(msprime_ibd_sf, "sf"))
  expect_true(!inherits(msprime_ibd_nosf, "sf"))

  # except for the spatial columns, the IBD results are the same
  expect_equal(as.data.frame(msprime_ibd_sf),
               as.data.frame(msprime_ibd_nosf))
})

test_that("ts_ibd() on spatial SLiM/slendr tree sequences works properly (with CRS)", {
  map <- readRDS("map.rds")
  p <- population(competition = 10e3, mating = 10e3, dispersal = 10e3, name = "pop1",
                  N = 1000, time = 3000, radius = 600000, center = c(10, 25), map = map)
  ts <- compile_model(populations = list(p), generation_time = 30,
                      resolution = 10e3, direction = "backward") %>%
    slim(sequence_length = 1e6, recombination_rate = 1e-8, random_seed = 42)

  set.seed(42)
  samples <- sample(ts_samples(ts)$name, 20)

  suppressWarnings(ibd_sf <- ts_ibd(ts, coordinates = TRUE, within = samples))
  suppressWarnings(ibd_nosf <- ts_ibd(ts, coordinates = TRUE, within = samples, sf = FALSE))

  # returned object is of a sf class (or not), as requested by the user
  expect_s3_class(ibd_sf, "sf")
  expect_true(!inherits(ibd_nosf, "sf"))

  # the IBD sf object has the CRS attribute of the map itself
  expect_true(sf::st_crs(map) == sf::st_crs(ibd_sf))

  # spatial IBD links within the same individual give an EMPTY linestring with
  # no coordinates
  within_ibd <- ibd_sf %>% dplyr::filter(name1 == name2) %>% dplyr::select(connection)
  expect_true(nrow(sf::st_coordinates(within_ibd)) == 0)

  # except for the spatial columns, the IBD results are the same
  expect_true(all(dplyr::select(as.data.frame(ibd_sf),
                            -node1_location, -node2_location, -connection) ==
               as.data.frame(ibd_nosf)))
})

test_that("ts_read tracks a path to the tree-sequence file if loaded from a file", {
  ts1 <- ts_read(slim_ts, model)
  ts2 <- ts_recapitate(ts1, recombination_rate = 0, Ne = 100)
  ts3 <- ts_simplify(ts2)
  ts4 <- ts_mutate(ts1, 1e-8)

  expect_true(attr(ts1, "path") == normalizePath(slim_ts))
  expect_true(attr(ts2, "path") == normalizePath(slim_ts))
  expect_true(attr(ts3, "path") == normalizePath(slim_ts))
  expect_true(attr(ts4, "path") == normalizePath(slim_ts))
})

test_that("ts_read does not track any file path if a tree sequence is directly loaded", {
  nonser_model <- model
  nonser_model$path <- NULL

  ts1 <- msprime(nonser_model, sequence_length = 100000, recombination_rate = 0,
                 random_seed = 314159, samples = samples)

  suppressWarnings(ts2 <- ts_recapitate(ts1, recombination_rate = 0, Ne = 100))
  ts3 <- ts_simplify(ts2)
  ts4 <- ts_mutate(ts1, 1e-8)

  expect_true(is.null(attr(ts1, "path")))
  expect_true(is.null(attr(ts2, "path")))
  expect_true(is.null(attr(ts3, "path")))
  expect_true(is.null(attr(ts4, "path")))
})

#------------------------------------------------------------------
# testing names extraction

ts_msprime <- ts_read(msprime_ts, model)
ts_slim <- ts_read(slim_ts, model)

test_that("ts_names(ts) returns a simple character vector", {
  expect_type(ts_names(ts_msprime), "character")
  expect_type(ts_names(ts_slim), "character")

  expect_true(length(ts_names(ts_msprime)) == sum(samples$n))
  expect_true(length(ts_names(ts_slim)) == sum(samples$n))
})

test_that("ts_names(ts, split = 'pop') returns a list of character vectors", {
  expect_type(ts_names(ts_msprime, split = "pop"), "list")
  expect_type(ts_names(ts_slim, split = "pop"), "list")

  expect_true(length(ts_names(ts_msprime, split = "pop")) == 2)
  expect_true(length(ts_names(ts_slim, split = "pop")) == 2)
})

test_that("ts_names(ts, split = 'time') returns a list of character vectors", {
  expect_type(ts_names(ts_msprime, split = "time"), "list")
  expect_type(ts_names(ts_slim, split = "time"), "list")

  expect_true(length(ts_names(ts_msprime, split = "time")) == length(unique(samples$time)))
  expect_true(length(ts_names(ts_slim, split = "time")) == length(unique(samples$time)))
})
