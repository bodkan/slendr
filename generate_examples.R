devtools::load_all(".")

init_env()

set.seed(314159)

# create the ancestor of everyone and a chimpanzee outgroup
# (we set both N = 1 to reduce the computational time for this model)
chimp <- population("CH", time = 6.5e6, N = 10)

# two populations of anatomically modern humans: Africans and Europeans
afr <- population("AFR", parent = chimp, time = 6e6, N = 10)
eur <- population("EUR", parent = afr, time = 70e3, N = 500)

# Neanderthal population splitting at 600 ky ago from modern humans
# (becomes extinct by 40 ky ago)
nea <- population("NEA", parent = afr, time = 600e3, N = 10, remove = 40e3)

# 3% Neanderthal introgression into Europeans between 55-50 ky ago
gf <- gene_flow(from = nea, to = eur, rate = 0.03, start = 55000, end = 45000)

model <- compile_model(
  populations = list(chimp, nea, afr, eur), gene_flow = gf,
  generation_time = 30,
  path = "inst/extdata/models/introgression", overwrite = TRUE, force = TRUE
)

# plot_model(model, sizes = FALSE, order = c("CH", "EUR", "NEA", "AFR"), log = TRUE)

nea_samples <- schedule_sampling(model, times = c(70000, 40000), list(nea, 1))
present_samples <- schedule_sampling(model, times = 0, list(chimp, 1), list(afr, 5), list(eur, 5))

# SLiM tree sequence
ts_slim <- slim(
  model, sequence_length = 0.5e6, recombination_rate = 1e-8,
  samples = rbind(nea_samples, present_samples),
  random_seed = 314159, verbose = TRUE
)

ts_slim %>%
  ts_recapitate(Ne = 10, recombination_rate = 1e-8) %>%
  ts_simplify(keep_input_roots = TRUE) %>%
  ts_save("inst/extdata/models/introgression_slim.trees")

# msprime tree sequence
ts_msprime <- msprime(
  model, sequence_length = 0.5e6, recombination_rate = 1e-8,
  samples = rbind(nea_samples, present_samples),
  random_seed = 314159, verbose = TRUE
)

ts_msprime %>% ts_save("inst/extdata/models/introgression_msprime.trees")

# generate a non-slendr msprime tree sequence
py_cmd <- sprintf("import msprime; msprime.sim_ancestry(%d, random_seed=42, population_size=%d).dump('%s')",
                  1000, 1000, "inst/extdata/models/msprime.trees")
reticulate::py_run_string(py_cmd)
