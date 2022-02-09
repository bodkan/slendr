env_present <- function(env) {
  "automatic_slendr_python_env" %in% reticulate::conda_list()$name
}

# Function used in unit tests verifying the correct number of individuals after
# various demographic changes
run_sim <- function(pop, direction, sim_length = NULL, method = "batch", verbose = FALSE) {
  model_dir <- tempdir()

  model <- compile(
    populations = list(pop), generation_time = 1,
    resolution = 10e3, competition_dist = 130e3, mate_dist = 100e3, dispersal_dist = 70e3,
    dir = model_dir, direction = direction, sim_length = sim_length, overwrite = TRUE
  )

  slim(model, sequence_length = 1, recombination_rate = 0, save_locations = TRUE,
       method = method, verbose = verbose)

  df <- suppressMessages(readr::read_tsv(file.path(model$path, "output_ind_locations.tsv.gz"),
                                         progress = FALSE)) %>%
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
  slim(forward_model, sequence_length = seq_len, recombination_rate = rec_rate,
       sampling = forward_samples, random_seed = seed, verbose = verbose)
  suppressWarnings({
    msprime(forward_model, sequence_length = seq_len, recombination_rate = rec_rate,
          sampling = forward_samples, random_seed = seed, verbose = verbose)
  })

  slim(backward_model, sequence_length = seq_len, recombination_rate = rec_rate,
       sampling = backward_samples, random_seed = seed, verbose = verbose)
  suppressWarnings({
  msprime(backward_model, sequence_length = seq_len, recombination_rate = rec_rate,
          sampling = backward_samples, random_seed = seed, verbose = verbose)
  })
}

load_tree_sequence <- function(backend, model, N, rec_rate, mut_rate, seed) {
  if (backend == "SLiM")
    ts_load(
      model, file = file.path(model$path, "output_slim.trees"), recapitate = TRUE, simplify = TRUE, mutate = TRUE,
      Ne = N, recombination_rate = rec_rate, mutation_rate = mut_rate,
      random_seed = seed
    )
  else
    ts_load(model, file = file.path(model$path, "output_msprime.trees"), mutate = TRUE, mutation_rate = mut_rate, random_seed = seed)
}
