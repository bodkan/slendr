env_present("retipy")

SEED <- 42
N <- 1000
n_samples <- 50

forward_pop <- population("forward_pop", time = 1, N = N, map = FALSE)
forward_model <- compile(forward_pop, "~/Desktop/msprime-forward", generation_time = 1,
                         overwrite = TRUE, direction = "forward", sim_length = 5000)

backward_pop <- population("backward_pop", time = 5000, N = N, map = FALSE)
backward_model <- compile(backward_pop, "~/Desktop/msprime-backward", generation_time = 1,
                          overwrite = TRUE, direction = "backward")

forward_samples <- sampling(forward_model, times = 5001, list(forward_pop, n_samples))
backward_samples <- sampling(backward_model, times = 0, list(backward_pop, n_samples))

slim(forward_model, sequence_length = 100e6, recombination_rate = 1e-8, sampling = forward_samples, seed = SEED)
msprime(forward_model, sequence_length = 100e6, recombination_rate = 1e-8, sampling = forward_samples, seed = SEED)

slim(backward_model, sequence_length = 100e6, recombination_rate = 1e-8, sampling = backward_samples, seed = SEED)
msprime(backward_model, sequence_length = 100e6, recombination_rate = 1e-8, sampling = backward_samples, seed = SEED)

reticulate::repl_python()

load_msprime_ts <- function(path) {
  ts <- tskit$load(path)
  ts <- msp$sim_mutations(
    ts,
    rate=1e-8,
    random_seed=123
  )
  ts
}

msprime_forward_ts <- load_msprime_ts("/Users/martin_petr/Desktop/msprime-forward/output_msprime.trees")
msprime_backward_ts <- load_msprime_ts("/Users/martin_petr/Desktop/msprime-backward/output_msprime.trees")

msprime_forward_afs <- msprime_forward_ts$allele_frequency_spectrum()
msprime_backward_afs <- msprime_backward_ts$allele_frequency_spectrum()

slim_forward_ts <- ts_load(
  forward_model, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
  Ne = N, recombination_rate = 1e-8, mutation_rate = 1e-8,
  random_seed = SEED
)
slim_backward_ts <- ts_load(
  backward_model, recapitate = TRUE, simplify = TRUE, mutate = TRUE,
  Ne = N, recombination_rate = 1e-8, mutation_rate = 1e-8,
  random_seed = SEED
)

slim_forward_afs <- ts_afs(slim_forward_ts)
slim_backward_afs <- ts_afs(slim_backward_ts)

n_afs <- 2 * n_samples + 1

df <- dplyr::tibble(
  sim = c(rep("msprime", 2 * n_afs), rep("SLiM", 2 * n_afs)),
  dir = c(rep("forward", n_afs), rep("backward", n_afs),
          rep("forward", n_afs), rep("backward", n_afs)),
  f = c(msprime_forward_afs, msprime_backward_afs,
        slim_forward_afs, slim_backward_afs)
)

ggplot(df, aes(f, color = sim, linetype = dir)) + geom_density() + facet_wrap(~ dir)
ggplot(df, aes(f, color = sim, linetype = dir)) + geom_density() + facet_wrap(~ sim)

ggplot() +
  geom_point(aes(msprime_forward_afs, msprime_backward_afs)) +
  coord_equal() +
  geom_abline(slope = 1)

expect_true(all(msprime_forward_afs == msprime_backward_afs))

ggplot() +
  geom_point(aes(slim_forward_afs, slim_backward_afs)) +
  coord_equal() +
  geom_abline(slope = 1)
expect_true(all(slim_forward_afs == slim_backward_afs))

ggplot() +
  geom_point(aes(msprime_forward_afs, slim_forward_afs)) +
  coord_equal() +
  geom_abline(slope = 1)
