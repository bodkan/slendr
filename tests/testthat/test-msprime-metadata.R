skip_if(!env_present("automatic_slendr_python_env")); setup_env(quiet = TRUE)

seed <- 42 # random seed
seq_len <- 2e5 # amount of sequence to simulate
rec_rate <- 1e-8 # uniform recombination rate
mut_rate <- 1e-8 # mutation rate

# forward models ----------------------------------------------------------

o <- population("o", time = 1, N = 1)
b <- population("b", parent = o, time = 500, N = 10)
c <- population("c", parent = b, time = 1000, N = 10)
x1 <- population("x1", parent = c, time = 2000, N = 10)
x2 <- population("x2", parent = c, time = 2000, N = 10)
a <- population("a", parent = b, time = 1500, N = 10)

forward_model_dir <- paste0(tempfile(), "_forward")

forward_model <- compile(populations = list(a, b, x1, x2, c, o), dir = forward_model_dir,
                 generation_time = 1, overwrite = TRUE, sim_length = 2200,
                 description = "The most incredible popgen model ever")

forward_samples <- rbind(
  sampling(forward_model, times = 2200, list(a, 1), list(b, 1), list(x1, 10), list(x2, 10), list(c, 1), list(o, 1)),
  sampling(forward_model, times = c(2000, 2050, 1123), list(a, 1), list(b, 1), list(x1, 10), list(x2, 10), list(c, 1), list(o, 1))
)

slim(forward_model, sequence_length = seq_len, recombination_rate = rec_rate, sampling = forward_samples, random_seed = seed)
msprime(forward_model, sequence_length = seq_len, recombination_rate = rec_rate, sampling = forward_samples, random_seed = seed)

forward_sts <- ts_load(forward_model, file = file.path(forward_model_dir, "output_slim.trees"))
forward_mts <- ts_load(forward_model, file = file.path(forward_model_dir, "output_msprime.trees"))

test_that("msprime and SLiM metadata is exactly the same (forward model)", {
  fields <- c("version", "description", "sampling")
  s_meta <- ts_metadata(forward_sts)[fields]
  m_meta <- ts_metadata(forward_mts)[fields]
  expect_true(all(sapply(fields, function(f) all(s_meta[[f]] == m_meta[[f]]))))

  args <- c("RECOMB_RATE", "SEED", "SEQUENCE_LENGTH", "SIMULATION_LENGTH")
  s_args <- ts_metadata(forward_sts)$arguments[args]
  m_args <- ts_metadata(forward_mts)$arguments[args]
  expect_true(all(sapply(args, function(f) all(s_args[[f]] == m_args[[f]]))))
})

test_that("msprime and SLiM sampling tables are exactly the same (forward model)", {
  expect_true(all(ts_samples(forward_sts) == ts_samples(forward_mts)))
})

test_that("sampling more individuals than is the current N triggers warning (forward model)", {
  forward_samples <- sampling(forward_model, times = c(2000, 2050, 1123), list(a, 1), list(x1, 1000), list(x2, 1000))
  expect_warning(
    slim(forward_model, sequence_length = seq_len, recombination_rate = rec_rate, sampling = forward_samples, random_seed = seed),
    "There were some warnings during the simulation run"
  )
})

# backward models ---------------------------------------------------------

o <- population("o", time = 2500, N = 1)
b <- population("b", parent = o, time = 2000, N = 10)
c <- population("c", parent = b, time = 1500, N = 10)
x1 <- population("x1", parent = c, time = 500, N = 10)
x2 <- population("x2", parent = c, time = 500, N = 10)
a <- population("a", parent = b, time = 1000, N = 10)

backward_model_dir <- paste0(tempfile(), "_backward")

backward_model <- compile(populations = list(a, b, x1, x2, c, o), dir = backward_model_dir,
                 generation_time = 1, overwrite = TRUE,
                 description = "The most incredible backward model ever")

# lets not fuss about the exact sampling -- this doesn't match exactly the
# forward model in the previous section, but we do want to make some samplings
# predate the appearance of certain populations to make the sampling schedule a
# bit more complex
# TODO: check what exactly happens if more individuals are scheduled for
# sampling than the number of individuals in the simulation being present (SLiM
# should only sample the maximum that is available, but what does msprime do
# here?)
backward_samples <- rbind(
  sampling(backward_model, times = 0, list(a, 1), list(b, 1), list(x1, 10), list(x2, 10), list(c, 1), list(o, 1)),
  sampling(backward_model, times = c(123, 250, 1000), list(a, 1), list(b, 1), list(x1, 10), list(x2, 10), list(c, 1), list(o, 1))
)

slim(backward_model, sequence_length = seq_len, recombination_rate = rec_rate, sampling = backward_samples, random_seed = seed)
msprime(backward_model, sequence_length = seq_len, recombination_rate = rec_rate, sampling = backward_samples, random_seed = seed)

backward_sts <- ts_load(backward_model, file = file.path(backward_model_dir, "output_slim.trees"))
backward_mts <- ts_load(backward_model, file = file.path(backward_model_dir, "output_msprime.trees"))

test_that("msprime and SLiM metadata is exactly the same (backward model)", {
  fields <- c("version", "description", "sampling")
  s_meta <- ts_metadata(backward_sts)[fields]
  m_meta <- ts_metadata(backward_mts)[fields]
  expect_true(all(sapply(fields, function(f) all(s_meta[[f]] == m_meta[[f]]))))

  args <- c("RECOMB_RATE", "SEED", "SEQUENCE_LENGTH", "SIMULATION_LENGTH")
  s_args <- ts_metadata(backward_sts)$arguments[args]
  m_args <- ts_metadata(backward_mts)$arguments[args]
  expect_true(all(sapply(args, function(f) all(s_args[[f]] == m_args[[f]]))))
})

test_that("msprime and SLiM sampling tables are exactly the same (backward model)", {
  expect_true(all(ts_samples(backward_sts) == ts_samples(backward_mts)))
})

test_that("sampling more individuals than is the current N triggers warning (backward model)", {
  backward_samples <- sampling(backward_model, times = c(123, 250, 1000), list(a, 1000), list(x1, 10), list(x2, 10), list(c, 1000))
  expect_warning(
    slim(backward_model, sequence_length = seq_len, recombination_rate = rec_rate, sampling = backward_samples, random_seed = seed),
    "There were some warnings during the simulation run"
  )
})
