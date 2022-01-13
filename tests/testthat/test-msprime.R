env_present("retipy")

seed <- 42
N <- 1000
n_samples <- 50
seq_len <- 200e6
rec_rate <- 1e-8
mut_rate <- 1e-8

# constant population size models - forward and backward direction, SLiM and msprime

forward_const_dir <- file.path(tempdir(), "forward_const")
forward_const_pop <- population("forward_const_pop", time = 1, N = N, map = FALSE)
forward_const_model <- compile(forward_const_pop, forward_const_dir, generation_time = 1,
                               overwrite = TRUE, direction = "forward", sim_length = 5000)
forward_const_samples <- sampling(forward_const_model, times = 5001, list(forward_const_pop, n_samples))

backward_const_dir <- file.path(tempdir(), "backward_const")
backward_const_pop <- population("backward_const_pop", time = 5000, N = N, map = FALSE)
backward_const_model <- compile(backward_const_pop, backward_const_dir , generation_time = 1,
                                overwrite = TRUE, direction = "backward")
backward_const_samples <- sampling(backward_const_model, times = 0, list(backward_const_pop, n_samples))

run_slim_msprime(
  forward_const_model, backward_const_model,
  forward_const_samples, backward_const_samples,
  seq_len, rec_rate, seed, verbose = FALSE
)

# population size contraction models - forward and backward direction, SLiM and msprime

forward_contr_dir <- file.path(tempdir(), "forward_contr")
forward_contr_pop <- population("forward_contr_pop", time = 1, N = N, map = FALSE) |>
  resize(time = 2001, N = 200, how = "step")
forward_contr_model <- compile(forward_contr_pop, forward_contr_dir, generation_time = 1,
                               overwrite = TRUE, direction = "forward", sim_length = 5000)
forward_contr_samples <- sampling(forward_contr_model, times = 5001, list(forward_contr_pop, n_samples))

backward_contr_dir <- file.path(tempdir(), "backward_contr")
backward_contr_pop <- population("backward_contr_pop", time = 5000, N = N, map = FALSE) |>
  resize(time = 3000, N = 200, how = "step")
backward_contr_model <- compile(backward_contr_pop, backward_contr_dir, generation_time = 1,
                                overwrite = TRUE, direction = "backward")
backward_contr_samples <- sampling(backward_contr_model, times = 0, list(backward_contr_pop, n_samples))

run_slim_msprime(
  forward_contr_model, backward_contr_model,
  forward_contr_samples, backward_contr_samples,
  seq_len, rec_rate, seed, verbose = FALSE
)


# population size increase models - forward and backward direction, SLiM and msprime

forward_expansion_dir <- file.path(tempdir(), "forward_expansion")
forward_expansion_pop <- population("forward_expansion_pop", time = 1, N = N, map = FALSE) |>
  resize(time = 2001, N = 5*N, how = "step")
forward_expansion_model <- compile(forward_expansion_pop, forward_expansion_dir, generation_time = 1,
                                   overwrite = TRUE, direction = "forward", sim_length = 5000)
forward_expansion_samples <- sampling(forward_expansion_model, times = 5001, list(forward_expansion_pop, n_samples))

backward_expansion_dir <- file.path(tempdir(), "backward_expansion")
backward_expansion_pop <- population("backward_expansion_pop", time = 5000, N = N, map = FALSE) |>
  resize(time = 3000, N = 5*N, how = "step")
backward_expansion_model <- compile(backward_expansion_pop, backward_expansion_dir, generation_time = 1,
                                    overwrite = TRUE, direction = "backward")
backward_expansion_samples <- sampling(backward_expansion_model, times = 0, list(backward_expansion_pop, n_samples))

run_slim_msprime(
  forward_expansion_model, backward_expansion_model,
  forward_expansion_samples, backward_expansion_samples,
  seq_len, rec_rate, seed, verbose = FALSE
)


# load tree sequence files from msprime
msprime_forward_const_ts <- load_msprime_ts(file.path(forward_const_model$path, "output_msprime.trees"))
msprime_backward_const_ts <- load_msprime_ts(file.path(backward_const_model$path, "output_msprime.trees"))

msprime_forward_contr_ts <- load_msprime_ts(file.path(forward_contr_model$path, "output_msprime.trees"))
msprime_backward_contr_ts <- load_msprime_ts(file.path(backward_contr_model$path, "output_msprime.trees"))

msprime_forward_expansion_ts <- load_msprime_ts(file.path(forward_expansion_model$path, "output_msprime.trees"))
msprime_backward_expansion_ts <- load_msprime_ts(file.path(backward_expansion_model$path, "output_msprime.trees"))

# load tree sequence files from SLiM
slim_forward_const_ts <- load_slim_ts(forward_const_model, N, rec_rate, mut_rate, seed)
slim_backward_const_ts <- load_slim_ts(backward_const_model, N, rec_rate, mut_rate, seed)

slim_forward_contr_ts <- load_slim_ts(forward_contr_model, N, rec_rate, mut_rate, seed)
slim_backward_contr_ts <- load_slim_ts(backward_contr_model, N, rec_rate, mut_rate, seed)

slim_forward_expansion_ts <- load_slim_ts(forward_expansion_model, N, rec_rate, mut_rate, seed)
slim_backward_expansion_ts <- load_slim_ts(backward_expansion_model, N, rec_rate, mut_rate, seed)

# compute AFS from all tree sequence files - msprime
msprime_forward_const_afs <- msprime_forward_const_ts$allele_frequency_spectrum(polarised = TRUE, span_normalise = FALSE)[-1]
msprime_backward_const_afs <- msprime_backward_const_ts$allele_frequency_spectrum(polarised = TRUE, span_normalise = FALSE)[-1]

msprime_forward_contr_afs <- msprime_forward_contr_ts$allele_frequency_spectrum(polarised = TRUE, span_normalise = FALSE)[-1]
msprime_backward_contr_afs <- msprime_backward_contr_ts$allele_frequency_spectrum(polarised = TRUE, span_normalise = FALSE)[-1]

msprime_forward_expansion_afs <- msprime_forward_expansion_ts$allele_frequency_spectrum(polarised = TRUE, span_normalise = FALSE)[-1]
msprime_backward_expansion_afs <- msprime_backward_expansion_ts$allele_frequency_spectrum(polarised = TRUE, span_normalise = FALSE)[-1]

# compute AFS from all tree sequence files - SLiM
slim_forward_const_afs <- ts_afs(slim_forward_const_ts, polarised = TRUE, span_normalise = FALSE)[-1]
slim_backward_const_afs <- ts_afs(slim_backward_const_ts, polarised = TRUE, span_normalise = FALSE)[-1]

slim_forward_contr_afs <- ts_afs(slim_forward_contr_ts, polarised = TRUE, span_normalise = FALSE)[-1]
slim_backward_contr_afs <- ts_afs(slim_backward_contr_ts, polarised = TRUE, span_normalise = FALSE)[-1]

slim_forward_expansion_afs <- ts_afs(slim_forward_expansion_ts, polarised = TRUE, span_normalise = FALSE)[-1]
slim_backward_expansion_afs <- ts_afs(slim_backward_expansion_ts, polarised = TRUE, span_normalise = FALSE)[-1]

afs <- dplyr::bind_rows(
  dplyr::tibble(f = msprime_forward_const_afs, n = 1:(2 * n_samples), sim = "msprime", direction = "forward", model = "const"),
  dplyr::tibble(f = msprime_backward_const_afs, n = 1:(2 * n_samples), sim = "msprime", direction = "backward", model = "const"),

  dplyr::tibble(f = msprime_forward_contr_afs, n = 1:(2 * n_samples), sim = "msprime", direction = "forward", model = "contraction"),
  dplyr::tibble(f = msprime_backward_contr_afs, n = 1:(2 * n_samples), sim = "msprime", direction = "backward", model = "contraction"),

  dplyr::tibble(f = msprime_forward_expansion_afs, n = 1:(2 * n_samples), sim = "msprime", direction = "forward", model = "expansion"),
  dplyr::tibble(f = msprime_backward_expansion_afs, n = 1:(2 * n_samples), sim = "msprime", direction = "backward", model = "expansion"),

  dplyr::tibble(f = slim_forward_const_afs, n = 1:(2 * n_samples), sim = "slim", direction = "forward", model = "const"),
  dplyr::tibble(f = slim_backward_const_afs, n = 1:(2 * n_samples), sim = "slim", direction = "backward", model = "const"),

  dplyr::tibble(f = slim_forward_contr_afs, n = 1:(2 * n_samples), sim = "slim", direction = "forward", model = "contraction"),
  dplyr::tibble(f = slim_backward_contr_afs, n = 1:(2 * n_samples), sim = "slim", direction = "backward", model = "contraction"),

  dplyr::tibble(f = slim_forward_expansion_afs, n = 1:(2 * n_samples), sim = "slim", direction = "forward", model = "expansion"),
  dplyr::tibble(f = slim_backward_expansion_afs, n = 1:(2 * n_samples), sim = "slim", direction = "backward", model = "expansion")
) |>
  dplyr::mutate(sim = factor(sim, levels = c("slim", "msprime")))


test_that("msprime forward/backward sims give the same result", {
  expect_true({
    df <- afs[afs$sim == "msprime" & afs$model == "const", ]
    all(df[df$direction == "forward", "f"] == df[df$direction == "backward", "f"])
  })
  expect_true({
    df <- afs[afs$sim == "msprime" & afs$model == "contraction", ]
    all(df[df$direction == "forward", "f"] == df[df$direction == "backward", "f"])
  })
  expect_true({
    df <- afs[afs$sim == "msprime" & afs$model == "expansion", ]
    all(df[df$direction == "forward", "f"] == df[df$direction == "backward", "f"])
  })
})

test_that("SLiM forward/backward sims give the same result", {
  expect_true({
    df <- afs[afs$sim == "slim" & afs$model == "const", ]
    all(df[df$direction == "forward", "f"] == df[df$direction == "backward", "f"])
  })
  expect_true({
    df <- afs[afs$sim == "slim" & afs$model == "contraction", ]
    all(df[df$direction == "forward", "f"] == df[df$direction == "backward", "f"])
  })
  expect_true({
    df <- afs[afs$sim == "slim" & afs$model == "expansion", ]
    all(df[df$direction == "forward", "f"] == df[df$direction == "backward", "f"])
  })
})

# SLiM and msprime simulations from the same model give the same result
# (tested by comparing the distribution plots)
p <- ggplot(afs, aes(n, f, color = direction, linetype = sim)) +
  geom_line(stat = "identity") +
  facet_wrap(~ model)

output_png <- paste0(tempfile(), ".png")
ggsave(output_png, p, width = 8, height = 5)
first_output_png <- "afs.png"
# ggsave(first_output_png, p, width = 8, height = 5)

# make sure that the distributions as they were originally inspected and
# verified visually match the new distributions plot
test_that("AFS distributions from SLiM and msprime simulations match", {
  expect_true(tools::md5sum(output_png) == tools::md5sum(first_output_png))
})
