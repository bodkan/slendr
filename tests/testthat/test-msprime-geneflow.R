env_present("retipy")

seed <- 42
N <- 100
seq_len <- 250e6
rec_rate <- 1e-8
mut_rate <- 1e-8

o <- population("o", time = 1, N = N)
b <- population("b", parent = o, time = 2000, N = N)
c <- population("c", parent = b, time = 3000, N = N)
x1 <- population("x1", parent = c, time = 6000, N = 50*N)
x2 <- population("x2", parent = c, time = 6000, N = 50*N)
a <- population("a", parent = b, time = 7000, N = N)

# no gene flow model
model_nogf <- compile(populations = list(a, b, x1, x2, c, o), generation_time = 1, overwrite = TRUE, sim_length = 10000)

samples <- sampling(model_nogf, times = 10001, list(a, 1), list(b, 1), list(x1, 50), list(x2, 50), list(c, 1), list(o, 1))

slim(model_nogf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, verbose = TRUE)
msprime(model_nogf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, verbose = TRUE)

# model with gene flow
gf <- geneflow(from = b, to = x1, start = 9000, end = 9050, rate = 0.1)

model_gf <- compile(populations = list(a, b, x1, x2, c, o), geneflow = gf, generation_time = 1, overwrite = TRUE, sim_length = 10000)

samples <- sampling(model_gf, times = 10001, list(a, 1), list(b, 1), list(x1, 50), list(x2, 50), list(c, 1), list(o, 1))

slim(model_gf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, verbose = TRUE)
msprime(model_gf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, verbose = TRUE)

ts_nogf <- ts_load(model_nogf, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
                   Ne = N, recombination_rate = rec_rate, mutation_rate = mut_rate)

ts_gf <- ts_load(model_gf, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
                 Ne = N, recombination_rate = rec_rate, mutation_rate = mut_rate)

X <- ts_samples(ts_gf) %>% dplyr::filter(pop %in% c("x1", "x2")) %>% dplyr::pull(name)

slim_f4 <- dplyr::bind_rows(
  purrr::map_dfr(X, ~ ts_f4(ts_nogf, "c_1", .x, "b_1", "o_1")) %>% dplyr::mutate(model = "no gene flow"),
  purrr::map_dfr(X, ~ ts_f4(ts_gf, "c_1", .x, "b_1", "o_1")) %>% dplyr::mutate(model = "gene flow")
) %>% dplyr::mutate(pop = gsub("_\\d+$", "", X))

ggplot(slim_f4, aes(f4, fill = pop)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ model) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(y = "number of individuals",
       title = "f4 statistics calculated from simulated data",
       subtitle = "x1 population receives gene flow (in gene flow models), x2 never does")

slim_f4r <- dplyr::bind_rows(
  purrr::map_dfr(X, ~ ts_f4ratio(ts_nogf, .x, "a_1", "b_1", "c_1", "o_1")) %>% dplyr::mutate(model = "no gene flow"),
  purrr::map_dfr(X, ~ ts_f4ratio(ts_gf, .x, "a_1", "b_1", "c_1", "o_1")) %>% dplyr::mutate(model = "gene flow")
) %>% dplyr::mutate(pop = gsub("_\\d+$", "", X))

ggplot(slim_f4r, aes(alpha, fill = pop)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ model) +
  geom_vline(xintercept = 0.1, linetype = 2) +
  labs(y = "number of individuals", x = "ancestry proportion",
       title = "f4-ratio 'b' ancestry estimate calculated from simulated data",
       subtitle = "x1 population receives gene flow (in gene flow models), x2 never does")






ts_msprime_no_geneflow <- load_msprime_ts(file.path(model_no_geneflow$path, "output_msprime.trees"))
ts_msprime_geneflow <- load_msprime_ts(file.path(model_geneflow$path, "output_msprime.trees"))

extract_nodes <- function(ts) {
  pop_ids <- seq(0, ts$num_populations - 1)
  nodes <- dplyr::tibble(
    pop = sapply(pop_ids, function(i) ts$population(as.integer(i))$metadata$name),
    nodes = lapply(pop_ids, function(i) ts$samples(as.integer(i)))

  )
  nodes
}

get_nodes <- function(pop, ts) {
  if (is.character(pop))  {
    nodes <- extract_nodes(ts)
    result <- nodes[nodes$pop == pop, ]$nodes[[1]]
  } else
    result <- list(pop)
  return(result)
}

msprime_f4 <- function(ts, w, x, y, z) {
  w_nodes <- get_nodes(w, ts)
  x_nodes <- get_nodes(x, ts)
  y_nodes <- get_nodes(y, ts)
  z_nodes <- get_nodes(z, ts)
  ts$f4(sample_sets = list(w_nodes, x_nodes, y_nodes, z_nodes))
}

msprime_f4ratio <- function(ts, a, b, x, c, o) {
  num <- msprime_f4(ts, a, o, x, c)
  den <- msprime_f4(ts, a, o, b, c)
  num / den
}

x_nodes <- get_nodes("x", ts_msprime_geneflow)

msprime_f4_nogf <- purrr::map_dbl(x_nodes, ~ msprime_f4(ts_msprime_no_geneflow, "c", .x, "b", "o"))
msprime_f4_gf <- purrr::map_dbl(x_nodes, ~ msprime_f4(ts_msprime_geneflow, "c", .x, "b", "o"))

msprime_f4ratio_nogf <- purrr::map_dbl(x_nodes, ~ msprime_f4ratio(ts_msprime_no_geneflow, "a", "b", .x, "c", "o"))
msprime_f4ratio_gf <- purrr::map_dbl(x_nodes, ~ msprime_f4ratio(ts_msprime_geneflow, "a", "b", .x, "c", "o"))
