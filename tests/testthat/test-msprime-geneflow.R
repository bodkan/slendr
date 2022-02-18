skip_if(!env_present("automatic_slendr_python_env") || Sys.getenv("bodkan_test") != TRUE)
setup_env(quiet = TRUE)

# Let's start by defining a couple of parameters for our simulations
seed <- 42 # random seed
seq_len <- 250e6 # amount of sequence to simulate
rec_rate <- 1e-8 # uniform recombination rate
mut_rate <- 1e-8 # mutation rate

# Now we define a very simple population model. Note that the Ne of the
# populations `x1` and `x2` is set to be much higher than the rest. The Ne of
# the other populations is low to speed up the simulation (they won't affect the
# results anyway), but increasing the Ne of the `x1` and `x2` will ensure that
# the effect of the drift acting on those two populations will be minimal.
# Because we will simulate (and, later, measure) the proportion of ancestry from
# the population `c`` to `x1`, this will ensure that the ancestry proportion
# will not drift too far away from the expectation.
#
# (Of course, this is all done for demonstration purposes only and to speed up
# the SLiM simulations by making the Ne of the other populations smaller.)
o <- population("o", time = 10, N = 1)
b <- population("b", parent = o, time = 500, N = 10)
c <- population("c", parent = b, time = 1000, N = 10)
x1 <- population("x1", parent = c, time = 2000, N = 10000)
x2 <- population("x2", parent = c, time = 2000, N = 10000)
a <- population("a", parent = b, time = 1500, N = 10)

# no gene flow model
model_nogf <- compile(populations = list(a, b, x1, x2, c, o), generation_time = 1, overwrite = TRUE, sim_length = 2200)

samples <- sampling(model_nogf, times = 2200, list(a, 1), list(b, 1), list(x1, 50), list(x2, 50), list(c, 1), list(o, 1))

slim(model_nogf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, random_seed = seed)
msprime(model_nogf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, random_seed = seed)

# model with gene flow
gf <- geneflow(from = b, to = x1, start = 2100, end = 2150, rate = 0.1)

model_gf <- compile(populations = list(a, b, x1, x2, c, o), geneflow = gf, generation_time = 1, overwrite = TRUE, sim_length = 2200)

samples <- sampling(model_gf, times = 2200, list(a, 1), list(b, 1), list(x1, 50), list(x2, 50), list(c, 1), list(o, 1))

slim(model_gf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, random_seed = seed)
msprime(model_gf, sequence_length = seq_len, recombination_rate = rec_rate, sampling = samples, random_seed = seed)

# Load tree sequence files saved by the SLiM backend script from the two models:
slim_nogf <- ts_load(model_nogf, file = file.path(model_nogf$path, "output_slim.trees"), recapitate = TRUE, simplify = TRUE, mutate = TRUE,
                     Ne = 10, recombination_rate = rec_rate, mutation_rate = mut_rate, random_seed = seed)

slim_gf <- ts_load(model_gf, file = file.path(model_gf$path, "output_slim.trees"), recapitate = TRUE, simplify = TRUE, mutate = TRUE,
                   Ne = 10, recombination_rate = rec_rate, mutation_rate = mut_rate, random_seed = seed)

# Extract vector of names of the "test individuals" in populations `x1` and `x2`:
slim_individuals <- ts_samples(slim_gf) %>%
  dplyr::filter(pop %in% c("x1", "x2")) %>%
  dplyr::pull(name)

# Calculate f4-statistics on individuals of `x1` and `x2` populations using data
# from the two models (a model with no gene flow and a gene flow model):
df_slim_f4 <- rbind(
  purrr::map_dfr(slim_individuals, ~ ts_f4(slim_nogf, "c_1", .x, "b_1", "o_1")) %>% dplyr::mutate(model = "no gene flow"),
  purrr::map_dfr(slim_individuals, ~ ts_f4(slim_gf, "c_1", .x, "b_1", "o_1")) %>% dplyr::mutate(model = "gene flow")
) %>%
  dplyr::select(X, f4, model) %>%
  dplyr::mutate(simulator = "SLiM backend")

# Compute the proportions of `b` ancestry in `x1` (expected 10%) and `x2`
# (expected 0% because this population did not receive any gene flow from `b`):
df_slim_f4ratio <- rbind(
  ts_f4ratio(slim_nogf, slim_individuals, "a_1", "b_1", "c_1", "o_1") %>% dplyr::mutate(model = "no gene flow"),
  ts_f4ratio(slim_gf, slim_individuals, "a_1", "b_1", "c_1", "o_1") %>% dplyr::mutate(model = "gene flow")
) %>%
  dplyr::select(X, alpha, model) %>%
  dplyr::mutate(simulator = "SLiM backend")





# Load tree sequence files saved by the SLiM backend script from the two models:
msprime_nogf <- ts_load(model_nogf, file = file.path(model_nogf$path, "output_msprime.trees"), mutate = TRUE,
                        mutation_rate = mut_rate, random_seed = seed)

msprime_gf <- ts_load(model_gf, file = file.path(model_gf$path, "output_msprime.trees"), mutate = TRUE,
                      mutation_rate = mut_rate, random_seed = seed)

# Extract vector of names of the "test individuals" in populations `x1` and `x2`:
msprime_individuals <- ts_samples(msprime_gf) %>%
  dplyr::filter(pop %in% c("x1", "x2")) %>%
  dplyr::pull(name)

test_that("msprime and SLiM produce the same set of sampled individuals", {
  expect_equal(slim_individuals, msprime_individuals)
})

# Calculate f4-statistics on individuals of `x1` and `x2` populations using data
# from the two models (a model with no gene flow and a gene flow model):
df_msprime_f4 <- rbind(
  purrr::map_dfr(msprime_individuals, ~ ts_f4(msprime_nogf, "c_1", .x, "b_1", "o_1")) %>% dplyr::mutate(model = "no gene flow"),
  purrr::map_dfr(msprime_individuals, ~ ts_f4(msprime_gf, "c_1", .x, "b_1", "o_1")) %>% dplyr::mutate(model = "gene flow")
) %>%
  dplyr::select(X, f4, model) %>%
  dplyr::mutate(simulator = "msprime backend")

# Compute the proportions of `b` ancestry in `x1` (expected 10%) and `x2`
# (expected 0% because this population did not receive any gene flow from `b`):
df_msprime_f4ratio <- rbind(
  ts_f4ratio(msprime_nogf, msprime_individuals, "a_1", "b_1", "c_1", "o_1") %>% dplyr::mutate(model = "no gene flow"),
  ts_f4ratio(msprime_gf, msprime_individuals, "a_1", "b_1", "c_1", "o_1") %>% dplyr::mutate(model = "gene flow")
) %>%
  dplyr::select(X, alpha, model) %>%
  dplyr::mutate(simulator = "msprime backend")

# Now for the real test: we defined several population genetic models. If
# everything works as expected, this model should produce the same result (or
# nearly the same result, taking into account uncertainty and randomness)
# regardless of whether we run it through the SLiM forward simulation backend
# engine or msprime coalescent backend engine:

df_f4 <- rbind(df_slim_f4, df_msprime_f4) %>%
  dplyr::mutate(population = ifelse(grepl("x1_", X),
                                    "x1 (received gene flow)",
                                    "x2 (no gene flow)")) %>%
  as.data.frame()

current_f4_tsv <- paste0(tempfile(), ".tsv.gz")
readr::write_tsv(df_f4, current_f4_tsv, progress = FALSE)
original_f4_tsv <- "f4.tsv.gz"
# readr::write_tsv(df_f4, original_f4_tsv, progress = FALSE)
orig_df_f4 <- readr::read_tsv(original_f4_tsv, show_col_types = FALSE, progress = FALSE) %>%
  as.data.frame()

# p_f4 <- ggplot(df_f4, aes(f4, fill = population)) +
#   geom_histogram(bins = 50) +
#   facet_grid(simulator ~ model) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   labs(y = "number of individuals", x = "f4 statistic",
#        title = "f4 statistics calculated on simulated data",
#        subtitle = "Note that for f4 values ~0, the hypothesis of no gene flow can't be rejected") +
#   theme(legend.position = "bottom")
# ggsave("f4.png", p_f4, width = 8, height = 5)

test_that("f4 distributions from SLiM and msprime simulations match", {
  expect_equal(df_f4, orig_df_f4, tolerance = 1e-15)
})

df_f4ratio <- rbind(df_slim_f4ratio, df_msprime_f4ratio) %>%
  dplyr::mutate(population = ifelse(grepl("x1_", X),
                                    "x1 (received gene flow)",
                                    "x2 (no gene flow)")) %>%
  as.data.frame()

current_f4r_tsv <- paste0(tempfile(), ".tsv.gz")
readr::write_tsv(df_f4ratio, current_f4r_tsv, progress = FALSE)
original_f4r_tsv <- "f4ratio.tsv.gz"
# readr::write_tsv(df_f4ratio, original_f4r_tsv, progress = FALSE)
orig_df_f4ratio <- readr::read_tsv(original_f4r_tsv, show_col_types = FALSE, progress = FALSE) %>%
  as.data.frame()

# p_f4ratio <- ggplot(df_f4ratio, aes(alpha, fill = population)) +
#   geom_histogram(bins = 30) +
#   facet_grid(simulator ~ model) +
#   geom_vline(xintercept = 0.1, linetype = 2) +
#   labs(y = "number of individuals", x = "ancestry proportion (f4-ratio statistic)",
#        title = "f4-ratio estimate of 'b' ancestry calculated from simulated data",
#        subtitle = "Population 'x1' receives 10% gene flow (vertical dotted line)
# from 'b' in gene flow models, 'x2' never does") +
#   theme(legend.position = "bottom")
# ggsave("f4ratio.png", p_f4ratio, width = 8, height = 5)

test_that("f4-ratio distributions from SLiM and msprime simulations match", {
  expect_equal(df_f4ratio, orig_df_f4ratio, tolerance = 1e-15)
})

# Great! We got almost the same results, as expected! We can also inspect the
# variance of the statistics between different simulation back ends and see that
# they are, indeed, extremely similar between both simulators:
#
# df_f4 %>% dplyr::group_by(model, simulator) %>% dplyr::summarise(var(f4))
# df_f4ratio %>% dplyr::group_by(model, simulator) %>% dplyr::summarise(var(alpha))
