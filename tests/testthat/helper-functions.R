# conda create --name retipy tskit pyslim msprime
env_present <- function(env) {
  tryCatch({
    reticulate::use_condaenv(env, required = TRUE)
    return(TRUE)
  },
  error = function(cond) FALSE
  )
}

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
