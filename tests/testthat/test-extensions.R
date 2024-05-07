skip_if(!is_slendr_env_present())

init_env(quiet = TRUE)

# basic extension type and sanity checking --------------------------------

test_that("an valid extension snippet must be a string or a file", {
  msg <- "Extension can be either a \\(multi-line\\) R string, or a path to a file"

  afr <- population("AFR", time = 100, N = 20)
  eur <- population("EUR", time = 10, N = 20, parent = afr)

  expect_error(compile_model(populations = list(afr, eur), generation_time = 1, extension = 123), msg)
  expect_error(compile_model(populations = list(afr, eur), generation_time = 1, extension = TRUE), msg)
})

test_that("an valid extension snippet must be a valid SLiM code or an existing file", {
  msg <- "Extension does not appear to be a file path nor a string containing SLiM code"

  afr <- population("AFR", time = 100, N = 20)
  eur <- population("EUR", time = 10, N = 20, parent = afr)

  expect_error(compile_model(populations = list(afr, eur), generation_time = 1, extension = "/tmp/asdfqwe"), msg)
  expect_error(compile_model(populations = list(afr, eur), generation_time = 1, extension = r"(
    this isn't valid SLiM code
  )"), msg)

  expect_s3_class(compile_model(populations = list(afr, eur), generation_time = 1, extension = r"(
    initialize() { for the purpose of a regex this is valid SLiM code}
  )"), "slendr_model")
})

test_that("SLiM extension R string snippet is correctly embedded into the compiled script", {
  afr <- population("AFR", time = 100, N = 20)
  eur <- population("EUR", time = 10, N = 20, parent = afr)

  extension <- "initialize() { catn('Hello from the SLiM side!'); }"

  model <- compile_model(populations = list(afr, eur), generation_time = 1, extension = extension)

  compiled_script <- readLines(file.path(model$path, "script.slim"))
  expect_true(any(grepl("Hello from the SLiM side!", compiled_script)))

  log_output <- capture.output(slim(model, sequence_length = 100, recombination_rate = 0, verbose = TRUE, ts = FALSE))
  expect_true(any(grepl("Hello from the SLiM side!", log_output)))
})

test_that("SLiM extension file is correctly embedded into the compiled script", {
  afr <- population("AFR", time = 100, N = 20)
  eur <- population("EUR", time = 10, N = 20, parent = afr)

  extension <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
  writeLines("initialize() { catn('Hello from the SLiM side!'); }", extension)

  model <- compile_model(populations = list(afr, eur), generation_time = 1, extension = extension)

  compiled_script <- readLines(file.path(model$path, "script.slim"))
  expect_true(any(grepl("Hello from the SLiM side!", compiled_script)))

  log_output <- capture.output(slim(model, sequence_length = 100, recombination_rate = 0, verbose = TRUE, ts = FALSE))
  expect_true(any(grepl("Hello from the SLiM side!", log_output)))
})

# enforce initialization contents -----------------------------------------

test_that("SLiM extension file is correctly embedded into the compiled script", {
  msg <- "SLiM extension snippets must either contain no initialize"

  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(initialize() { initializeMutationType("m1", 0.5, "f", 0.0); })"
  expect_error(compile_model(populations = pop, generation_time = 1, extension = extension), msg)

  extension <- r"(initialize() { initializeMutationRate(1e-8); } )"
  expect_error(compile_model(populations = pop, generation_time = 1, extension = extension), msg)

  extension <- r"(initialize() { initializeRecombinationRate(1e-8); })"
  expect_error(compile_model(populations = pop, generation_time = 1, extension = extension), msg)

  extension <- r"(initialize() {
    initializeMutationRate(1e-8);
    initializeRecombinationRate(1e-8);
  })"
  expect_error(compile_model(populations = pop, generation_time = 1, extension = extension), msg)

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, SEQUENCE_LENGTH - 1);

    initializeMutationRate(0);
    initializeRecombinationRate(RECOMBINATION_RATE);
  })"
  expect_s3_class(compile_model(populations = pop, generation_time = 1, extension = extension), "slendr_model")
})

test_that("if simulation length is not given in a model, slim() requires it (non-custom script)", {
  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, SEQUENCE_LENGTH - 1);

    initializeMutationRate(0);
    initializeRecombinationRate(RECOMBINATION_RATE);
  })"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  msg <- "Specifying the `sequence_length =` argument is required"
  expect_error(slim(model), msg)
  expect_error(slim(model, recombination_rate = 1e-8), msg)
  expect_s3_class(slim(model, sequence_length = 100, recombination_rate = 0), "slendr_ts")
})

test_that("if recombination rate is not given in a model, slim() requires it (non-custom script)", {
  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, SEQUENCE_LENGTH - 1);

    initializeMutationRate(0);
    initializeRecombinationRate(RECOMBINATION_RATE);
  })"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  msg <- "Specifying the `recombination_rate =` argument is required"
  expect_error(slim(model, sequence_length = 1e-8), msg)
  expect_s3_class(slim(model, sequence_length = 100, recombination_rate = 0), "slendr_ts")
})

test_that("if simulation length is not given in a model, slim() requires it (customized script)", {
  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, SEQUENCE_LENGTH - 1);

    initializeMutationRate(0);
    initializeRecombinationRate(0);
  })"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  msg <- "Specifying the `sequence_length =` argument is required"
  expect_error(slim(model), msg)
  expect_s3_class(slim(model, sequence_length = 100), "slendr_ts")
})

test_that("if recombination rate is not given in a model, slim() requires it (customized script)", {
  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, 999);

    initializeMutationRate(0);
    initializeRecombinationRate(RECOMBINATION_RATE);
  })"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  msg <- "Specifying the `recombination_rate =` argument is required"
  expect_error(slim(model), msg)
  expect_s3_class(slim(model, recombination_rate = 0), "slendr_ts")
})

test_that("slim() does not require sequence length and recombination rate with custom scripts", {
  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, 999);

    initializeMutationRate(0);
    initializeRecombinationRate(1e-8);
  })"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  expect_s3_class(slim(model), "slendr_ts")
})

test_that("slim() does not accept sequence length and recombination rate with custom scripts", {
  pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, 999);

    initializeMutationRate(0);
    initializeRecombinationRate(1e-8);
  })"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  expect_error(slim(model, sequence_length = 1000),
               "The `sequence_length =` argument given even when length is specified")
  expect_error(slim(model, recombination_rate = 1e-8),
               "The `recombination_rate =` argument given even when the rate is specified")
  expect_s3_class(slim(model), "slendr_ts")
})


# output generation -------------------------------------------------------

pop <- population("pop", time = 100, N = 100) %>% resize(time = 10, N = 10, how = "step")

output_file <- normalizePath(tempfile(), winslash = "/", mustWork = FALSE)
extension <- sprintf(r"(
initialize() {
  initializeMutationType("m1", 0.5, "f", 0.0);

  initializeGenomicElementType("g1", m1, 1.0);
  initializeGenomicElement(g1, 0, 999);

  initializeMutationRate(0);
  initializeRecombinationRate(1e-8);
}

SIMULATION_LENGTH late() {
  writeFile("%s", "asdf");
}
)", output_file)

model <- compile_model(populations = pop, generation_time = 1, extension = extension)

test_that("output file and tree sequence are both there", {
  result <- slim(model)
  expect_s3_class(result, "slendr_ts")
  expect_true(file.exists(output_file))
  expect_true(readLines(output_file) == "asdf")
})

test_that("output file is there but tree sequence is not returned", {
  result <- slim(model, ts = FALSE)
  expect_true(result == "")
  expect_true(file.exists(output_file))
  expect_true(readLines(output_file) == "asdf")
})

test_that("output directory can be set and files and tree sequence are saved there", {
  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, 999);

    initializeMutationRate(0);
    initializeRecombinationRate(1e-8);
  }

  SIMULATION_LENGTH late() {
    path = OUTPUT_DIR + "/" + "output_file";
    writeFile(path, "asdf");
  }
  )"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  output_dir <- normalizePath(file.path(tempdir(), "testing_dir1"), winslash = "/", mustWork = FALSE)
  output_file <- file.path(output_dir, "output_file")
  result <- slim(model, output_dir = output_dir)

  # slim(..., output_dir = ...) returns the directory path
  expect_true(result == output_dir)
  expect_equal(sort(list.files(output_dir)), sort(c("output_file", "output.trees")))
  expect_true(file.exists(output_file))
  expect_true(readLines(output_file) == "asdf")
  expect_s3_class(ts_load(file.path(output_dir, "output.trees"), model), "slendr_ts")
})

test_that("output directory can be set and files (but no tree sequence) are saved there", {
  extension <- r"(
  initialize() {
    initializeMutationType("m1", 0.5, "f", 0.0);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, 999);

    initializeMutationRate(0);
    initializeRecombinationRate(1e-8);
  }

  SIMULATION_LENGTH late() {
    path = OUTPUT_DIR + "/" + "output_file";
    writeFile(path, "asdf");
  }
  )"

  model <- compile_model(populations = pop, generation_time = 1, extension = extension)

  output_dir <- normalizePath(file.path(tempdir(), "testing_dir2"), winslash = "/", mustWork = FALSE)
  output_file <- file.path(output_dir, "output_file")
  result <- slim(model, output_dir = output_dir, ts = FALSE)

  # slim(..., output_dir = ...) returns the directory path
  expect_true(result == output_dir)
  expect_equal(list.files(output_dir), "output_file")
  expect_true(file.exists(output_file))
  expect_true(readLines(output_file) == "asdf")
})
