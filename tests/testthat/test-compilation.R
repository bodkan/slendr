test_that("name of a population can only be a scalar character value", {
  error_msg <- "A population name must be a character scalar value"
  expect_error(population(name = c("asd", "xyz"), time = 1, N = 100), error_msg)
  expect_s3_class(population(name = "asd", time = 1, N = 100), "slendr_pop")
})

test_that("'competition' must be specified in compile_model() if missing", {
  map <- readRDS("map.rds")
  p <- population(mating = 10, dispersal = 10, name = "pop1", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  expect_error(compile_model(populations = list(p), generation_time = 30, resolution = 11e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"),
               "Parameter 'competition' missing", fixed = TRUE)
  skip_if(Sys.info()[["sysname"]] == "Windows") # new meaningless CRAN warnings
  expect_silent(compile_model(competition = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("'mating' must be specified in compile_model() if missing", {
  map <- readRDS("map.rds")
  p <- population(competition = 10, dispersal = 10, name = "pop1", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  skip_if(Sys.info()[["sysname"]] == "Windows") # new meaningless CRAN warnings
  expect_error(compile_model(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"),
               "Parameter 'mating' missing", fixed = TRUE)
  expect_silent(compile_model(mating = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("'dispersal' must be specified in compile_model() if missing", {
  map <- readRDS("map.rds")
  p <- population(competition = 10, mating = 10, name = "pop1", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  skip_if(Sys.info()[["sysname"]] == "Windows") # new meaningless CRAN warnings
  expect_error(compile_model(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"),
               "Parameter 'dispersal' missing", fixed = TRUE)
  expect_silent(compile_model(dispersal = 50e3, populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("'competition', 'mating', and 'dispersal' do not have to be specified in compile_model() if already present", {
  map <- readRDS("map.rds")
  p <- population(competition = 10, mating = 10, dispersal = 10, name = "pop1", N = 700, time = 40000, radius = 600000, center = c(10, 25), map = map)
  skip_if(Sys.info()[["sysname"]] == "Windows") # new meaningless CRAN warnings
  expect_silent(compile_model(populations = list(p), generation_time = 30, resolution = 10e3, path = tempfile(), overwrite = TRUE, force = TRUE, direction = "backward"))
})

test_that("presence of all parents is enforced", {
  p1 <- population(name = "pop1", N = 700, time = 40000)
  p2 <- population(name = "pop2", parent = p1, N = 700, time = 4000)
  expect_error(compile_model(populations = p2, path = file.path(tempdir(), "missing-parent"), generation_time = 30),
               "The following parent populations are missing: pop1")
})

test_that("invalid blank maps are prevented", {
  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")
  pop <- population("pop", time = 1, N = 100, map = map, center = c(50, 50), radius = 0.5)

  expect_error(compile_model(pop, generation_time = 1, competition = 1,
                       mating = 50, dispersal = 1, simulation_length = 300,
                       resolution = 1),
               "No occupiable pixel on a rasterized map")
})

test_that("deletion in non-interactive mode must be forced", {
  skip_if(interactive())
  p <- population(name = "pop", N = 700, time = 100) %>% resize(N = 100, time = 50, how = "step")
  directory <- file.path(tempdir(), "dir-forced")
  dir.create(directory)
  expect_error(model <- compile_model(p, path = directory, generation_time = 30, overwrite = TRUE),
               "Compilation aborted")
  model <- compile_model(p, path = directory, generation_time = 30, overwrite = TRUE, force = TRUE)
  expect_true(grepl("dir-forced$", model$path))
})

skip_if(!check_dependencies(python = TRUE))
init_env(quiet = TRUE)

test_that("sequence length can only be an integer number (SLiM)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Sequence length must be a non-negative integer number"
  expect_error(slim(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_error(slim(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_silent(slim(model, sequence_length = 1e6, recombination_rate = 0))
})

test_that("sequence length can only be an integer number (msprime)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Sequence length must be a non-negative integer number"
  expect_error(msprime(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_error(msprime(model, sequence_length = 0.1, recombination_rate = 0), error_msg)
  expect_silent(msprime(model, sequence_length = 1e6, recombination_rate = 0))
})

test_that("recombination rate can only be an integer number (SLiM)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Recombination rate must be a non-negative numeric value"
  expect_error(slim(model, sequence_length = 100, recombination_rate = "asdf"), error_msg)
  expect_error(slim(model, sequence_length = 100, recombination_rate = -1), error_msg)
  expect_s3_class(slim(model, sequence_length = 100, recombination_rate = 1e-8), "slendr_ts")
})

test_that("recombination rate can only be an integer number (msprime)", {
  p <- population(name = "pop1", N = 700, time = 1)
  model <- compile_model(populations = p, generation_time = 30, simulation_length = 1000)
  error_msg <- "Recombination rate must be a non-negative numeric value"
  expect_error(msprime(model, sequence_length = 100, recombination_rate = "asdf", random_seed = 42), error_msg)
  expect_error(msprime(model, sequence_length = 100, recombination_rate = -1, random_seed = 42), error_msg)
  expect_silent(msprime(model, sequence_length = 100, recombination_rate = 1e-8, random_seed = 42))
})

test_that("mix of spatial and non-spatial populations gives a warning", {
  map <- readRDS("map.rds")
  p1 <- population(name = "pop1", N = 700, map = map, time = 40000)
  p2 <- population(name = "pop2", N = 700, time = 4000)
  expect_warning(
    compile_model(populations = list(p1, p2), generation_time = 30, direction = "backward",
                  resolution = 10e3, competition = 10e3, mating = 10e3, dispersal = 10e3),
    "Model containing a mix of spatial and non-spatial populations"
  )
})

test_that("purely spatial populations compile in silence", {
  map <- readRDS("map.rds")
  p1 <- population(name = "pop1", N = 700, map = map, time = 40000)
  p2 <- population(name = "pop2", N = 700, map = map, time = 4000)
  expect_s3_class(
    compile_model(populations = list(p1, p2), generation_time = 30, direction = "backward",
                  resolution = 10e3, competition = 10e3, mating = 10e3, dispersal = 10e3),
    "slendr_model"
  )
})

test_that("purely non-spatial populations compile in silence", {
  p1 <- population(name = "pop1", N = 700, time = 40000)
  p2 <- population(name = "pop2", N = 700, time = 4000)
  expect_s3_class(
    compile_model(populations = list(p1, p2), generation_time = 30, direction = "backward"),
    "slendr_model"
  )
})

# tests of the extract_parameters function --------------------------------------

# first compile a model (taken from vignette number 4)

afr <- population("AFR", time = 52000, N = 3000)
ooa <- population("OOA", parent = afr, time = 51000, N = 500, remove = 25000) %>%
  resize(N = 2000, time = 40000, how = "step")
ehg <- population("EHG", parent = ooa, time = 28000, N = 1000, remove = 6000)
eur <- population("EUR", parent = ehg, time = 25000, N = 2000) %>%
  resize(N = 10000, how = "exponential", time = 5000, end = 0)
ana <- population("ANA", time = 28000, N = 3000, parent = ooa, remove = 4000)
yam <- population("YAM", time = 7000, N = 500, parent = ehg, remove = 2500)

gf <- list(
  gene_flow(from = ana, to = yam, rate = 0.5, start = 6500, end = 6400),
  gene_flow(from = ana, to = eur, rate = 0.5, start = 8000, end = 6000),
  gene_flow(from = yam, to = eur, rate = 0.75, start = 4000, end = 3000)
)

# full model
model <- compile_model(populations = list(afr, ooa, ehg, eur, ana, yam),
                       gene_flow = gf, generation_time = 30)

# no-gene-flow model
model_nogf <- compile_model(populations = list(afr, ooa, ehg, eur, ana, yam),
                            generation_time = 30)

# no-resizes model
ooa <- population("OOA", parent = afr, time = 51000, N = 500, remove = 25000)
eur <- population("EUR", parent = ehg, time = 25000, N = 2000)
model_noresizes <- compile_model(populations = list(afr, ooa, ehg, eur, ana, yam),
                                 gene_flow = gf, generation_time = 30)

# model with no dynamics
model_base <- compile_model(populations = list(afr, ooa, ehg, eur, ana, yam),
                            generation_time = 30)

test_that("parameters of the full model are extracted properly (from model)", {
  pars <- extract_parameters(model)
  expect_length(pars, 3)
  expect_equal(names(pars), c("splits", "gene_flows", "resizes"))
})

test_that("parameters of the no-gf model are extracted properly (from model)", {
  pars <- extract_parameters(model_nogf)
  expect_length(pars, 2)
  expect_equal(names(pars), c("splits", "resizes"))
})

test_that("parameters of the no-resize model are extracted properly (from model)", {
  pars <- extract_parameters(model_noresizes)
  expect_length(pars, 2)
  expect_equal(names(pars), c("splits", "gene_flows"))
})

test_that("parameters of the base model are extracted properly (from model)", {
  pars <- extract_parameters(model_base)
  expect_length(pars, 1)
  expect_equal(names(pars), "splits")
})

skip_if(!check_dependencies(python = TRUE))
init_env(quiet = TRUE)

ts <- msprime(model, sequence_length = 1, recombination_rate = 0)
ts_nogf <- msprime(model_nogf, sequence_length = 1, recombination_rate = 0)
ts_noresizes <- msprime(model_noresizes, sequence_length = 1, recombination_rate = 0)
ts_base <- msprime(model_base, sequence_length = 1, recombination_rate = 0)

test_that("parameters of the full model are extracted properly (from t.s.)", {
  pars <- extract_parameters(ts)
  expect_length(pars, 3)
  expect_equal(names(pars), c("splits", "gene_flows", "resizes"))
})

test_that("parameters of the no-gf model are extracted properly (from t.s.)", {
  pars <- extract_parameters(ts_nogf)
  expect_length(pars, 2)
  expect_equal(names(pars), c("splits", "resizes"))
})

test_that("parameters of the no-resize model are extracted properly (from t.s.)", {
  pars <- extract_parameters(ts_noresizes)
  expect_length(pars, 2)
  expect_equal(names(pars), c("splits", "gene_flows"))
})

test_that("parameters of the base model are extracted properly (from t.s.)", {
  pars <- extract_parameters(ts_base)
  expect_length(pars, 1)
  expect_equal(names(pars), "splits")
})

test_that("extract_parameters fails gracefully when non-slendr tree sequence is used", {
  output <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  py_cmd <- sprintf("import msprime; msprime.sim_ancestry(%d, random_seed=42, population_size=%d).dump('%s')", 10, 10, output)
  reticulate::py_run_string(py_cmd)
  ts <- ts_read(output)
  expect_error(extract_parameters(ts), "No slendr model configuration present in the tree sequence.")
})

