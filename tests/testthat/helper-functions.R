# Function used in unit tests verifying the correct number of individuals after
# various demographic changes
run_sim <- function(pop, direction, simulation_length = NULL, method = "batch", verbose = FALSE) {
  model_dir <- tempdir()

  model <- compile_model(
    populations = list(pop), generation_time = 1,
    resolution = 10e3, competition = 130e3, mating = 100e3, dispersal = 70e3,
    path = model_dir, direction = direction, simulation_length = simulation_length, overwrite = TRUE, force = TRUE
  )

  locations_file <- normalizePath(tempfile(fileext = ".gz"), winslash = "/", mustWork = FALSE)
  slim(model, sequence_length = 1, recombination_rate = 0,,
       method = method, verbose = verbose, locations = locations_file)

  df <- suppressMessages(readr::read_tsv(locations_file, progress = FALSE)) %>%
    dplyr::mutate(time = convert_slim_time(gen, model)) %>%
    dplyr::group_by(gen, time, pop) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "keep")

  if (direction == "forward")
    df <- dplyr::arrange(df, time)
  else
    df <- dplyr::arrange(df, -time)

  df
}

# Function used to cross-test the consistency of msprime and SLiM simulations
# executed by the two slendr backends on the same slendr model configuration
run_slim_msprime <- function(forward_model, backward_model,
                             forward_samples, backward_samples,
                             seq_len, rec_rate, seed, verbose) {
  ts_slim_forward <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  ts_msprime_forward <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)

  slim(forward_model, sequence_length = seq_len, recombination_rate = rec_rate,
       samples = forward_samples, random_seed = seed, verbose = verbose) %>% ts_write(ts_slim_forward)
  suppressWarnings({
    msprime(forward_model, sequence_length = seq_len, recombination_rate = rec_rate,
          samples = forward_samples, random_seed = seed, verbose = verbose) %>% ts_write(ts_msprime_forward)
  })

  ts_slim_backward <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  ts_msprime_backward <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)

  slim(backward_model, sequence_length = seq_len, recombination_rate = rec_rate,
       samples = backward_samples, random_seed = seed, verbose = verbose) %>% ts_write(ts_slim_backward)
  suppressWarnings({
  msprime(backward_model, sequence_length = seq_len, recombination_rate = rec_rate,
          samples = backward_samples, random_seed = seed, verbose = verbose) %>% ts_write(ts_msprime_backward)
  })

  list(
    "slim_forward"     = ts_slim_forward,
    "msprime_forward"  = ts_msprime_forward,
    "slim_backward"    = ts_slim_backward,
    "msprime_backward" = ts_msprime_backward
  )
}

load_tree_sequence <- function(backend, direction, ts_list, model, N, rec_rate, mut_rate, seed) {
  ts_file <- ts_list[[paste(tolower(backend), direction, sep = "_")]]
  if (backend == tolower("SLiM"))
    ts_read(model = model, file = ts_file) %>%
      ts_recapitate(Ne = N, recombination_rate = rec_rate, random_seed = seed) %>%
      ts_simplify() %>%
      ts_mutate(mutation_rate = mut_rate, random_seed = seed)
  else
    ts_read(model = model, file = ts_file) %>%
      ts_mutate(mutation_rate = mut_rate, random_seed = seed)
}
