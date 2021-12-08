# conda create --name retipy tskit pyslim msprime
env_present <- function(env) {
  tryCatch({
    reticulate::use_condaenv(env, required = TRUE)
    return(TRUE)
  },
  error = function(cond) FALSE
  )
}
