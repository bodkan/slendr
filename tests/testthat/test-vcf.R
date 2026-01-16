skip_on_cran()
skip_if(!check_dependencies(python = TRUE))

library(slendr)
init_env(quiet = TRUE)

pop <- population("pop", time = 1000, N = 100)
model <- compile_model(pop, generation_time = 1, direction = "backward", serialize = TRUE)
schedule <- schedule_sampling(model, times = 0, list(pop, 10))

# give warning on missing mutations ---------------------------------------

test_that("exporting VCF from tree sequences without mutations gives warning (slendr ts)", {
  ts_nomuts <- msprime(model, sequence_length = 1000000, recombination_rate = 0, samples = schedule)
  ts_muts <- ts_nomuts %>% ts_mutate(1e-8)

  vcf_path <- tempfile()
  expect_warning(ts_vcf(ts_nomuts, path = vcf_path), "Attempting to extract genotypes")

  vcf_path <- tempfile()
  expect_silent(ts_vcf(ts_muts, path = vcf_path))
  expect_true(file.exists(vcf_path))
})

test_that("exporting VCF from tree sequences without mutations gives warning (non-slendr ts)", {
  ts_nomuts <- msprime(model, sequence_length = 1000000, recombination_rate = 0, samples = schedule) %>% ts_read
  ts_muts <- ts_nomuts %>% ts_mutate(1e-8)

  vcf_path <- tempfile()
  expect_warning(ts_vcf(ts_nomuts, path = vcf_path), "Attempting to extract genotypes")

  vcf_path <- tempfile()
  expect_silent(ts_vcf(ts_muts, path = vcf_path))
  expect_true(file.exists(vcf_path))
})


# give error on missing individuals ---------------------------------------

test_that("writing VCF from missing individuals leads to an error (slendr ts)", {
  ts <- msprime(model, sequence_length = 1000000, recombination_rate = 0, samples = schedule) %>%
    ts_mutate(1e-8)

  vcf_path <- tempfile()
  expect_error(ts_vcf(ts, path = vcf_path, individuals = c("we", "pop_7", "are", "not", "pop_9", "here", "pop_1")),
               "The following individuals are not present in the tree sequence: we, are, not, here")

  vcf_path <- tempfile()
  expect_silent(ts_vcf(ts, path = vcf_path, individuals = c("pop_1", "pop_10")))
  expect_true(file.exists(vcf_path))
})

test_that("writing VCF from missing individuals leads to an error (non-slendr ts)", {
  ts <- msprime(model, sequence_length = 1000000, recombination_rate = 0, samples = schedule) %>%
    ts_mutate(1e-8) %>% ts_read

  vcf_path <- tempfile()
  expect_error(ts_vcf(ts, path = vcf_path, individuals = c(0, 5, "we", "pop_7", "are", "not", "pop_9", "here", "pop_1")),
               "For non-slendr tree sequences, all individual identifiers must be numeric")

  vcf_path <- tempfile()
  expect_error(ts_vcf(ts, path = vcf_path, individuals = c(0, 3, 5, 123, 450)),
               "The following individuals are not present in the tree sequence: 123, 450")

  vcf_path <- tempfile()
  expect_silent(ts_vcf(ts, path = vcf_path, individuals = c(0, 3, 5)))
  expect_true(file.exists(vcf_path))
})
