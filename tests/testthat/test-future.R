# This script tests that models with times going "into the future" give the same result when
# run through both SLiM and msprime. This has been prompted by strange errors like
# "negative times not valid" from msprime when running models like this. This was caused
# by convert_to_forward() missing the following clause (ignoring special treatment of
# "forward" direction models entirely):
#     else if (direction == "forward")
#        times[times != -1] <- times[times != -1] - start_time + generation_time
# The reason why this wasn't discovered earlier is that the change only manifests in more
# complicated forward models (and only when run through msprime).
#
# These kinds of unit tests are also done elsewhere but those clearly have not been
# exhaustive enough -- it's better to put new tests in a separate script so that it's
# cleare why have these been introduced.

skip_on_cran()
skip_if(!check_dependencies(python = TRUE))

init_env(quiet = TRUE)

# resizes with default sampling ---------------------------------------------------------------

test_that("forward models not starting from 1 given the same outcome (resizes, all samples)", {
  pop1 <- population("pop1", time = 1500, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 20, simulation_length = 1000)

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

test_that("forward models starting from 1 give the same outcome (resizes, all samples)", {
  pop1 <- population("pop1", time = 1, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 20, simulation_length = 3000)

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

test_that("(truncated) forward models starting from 1 give the same outcome (resizes, all samples)", {
  pop1 <- population("pop1", time = 1, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 20, simulation_length = 1000)

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

# resizes with defined sampling ---------------------------------------------------------------

test_that("forward models not starting from 1 give the same outcome (resizes, defined samples)", {
  pop1 <- population("pop1", time = 1500, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 20, simulation_length = 1000)

  samples <- schedule_sampling(model, times = c(1, 500, 1900, 2200, 2500), list(pop1, 2), list(pop2, 2))

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

test_that("forward models not starting from 1 give the same outcome (resizes, defined samples)", {
  pop1 <- population("pop1", time = 1, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 20, simulation_length = 3000)

  samples <- schedule_sampling(model, times = c(1, 500, 1000, 1001, 1900, 2200, 2500), list(pop1, 2), list(pop2, 2))

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

# gene-flow events ---------------------------------------------------------------

test_that("forward models not starting from 1 given the same outcome (gene flows)", {
  pop1 <- population("pop1", time = 1500, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  gf <- gene_flow(pop1, pop2, proportion = 0.1, start = 1800, end = 1900)

  model <- compile_model(list(pop1, pop2), gene_flow = gf, generation_time = 20, simulation_length = 1000)

  samples <- schedule_sampling(model, times = c(1, 500, 1900, 2200, 2500), list(pop1, 2), list(pop2, 2))

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

test_that("forward models starting from 1 given the same outcome (gene flows)", {
  pop1 <- population("pop1", time = 1, N = 100) %>%
    resize(time = 1800, N = 10, how = "step") %>%
    resize(time = 2024, N = 30, how = "step") %>%
    resize(time = 2200, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 100, parent = pop1) %>%
    resize(time = 1800, N = 10, how = "step")

  gf <- gene_flow(pop1, pop2, proportion = 0.1, start = 1800, end = 1900)

  model <- compile_model(list(pop1, pop2), gene_flow = gf, generation_time = 20, simulation_length = 1000)

  samples <- schedule_sampling(model, times = c(1, 500, 1900, 2200, 2500), list(pop1, 2), list(pop2, 2))

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8, samples = samples)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})


# backwards models ----------------------------------------------------------------------------

# This is out of the scope of this unit-test script but given the changes introduced for
# the bugfixes described above, let's make sure everything still works as it used to
# (namely that truncated backwards-time models correctly schedule default sampling times).

test_that("backward models correctly sample by default 'at present' (non-truncated)", {
  pop1 <- population("pop1", time = 10000, N = 100) %>%
    resize(time = 7800, N = 10, how = "step") %>%
    resize(time = 4024, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 50, parent = pop1) %>%
    resize(time = 800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 100)

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})

test_that("backward models correctly sample by default 'at present' (truncated)", {
  pop1 <- population("pop1", time = 10000, N = 100) %>%
    resize(time = 7800, N = 10, how = "step") %>%
    resize(time = 4024, N = 30, how = "step")
  pop2 <- population("pop2", time = 1700, N = 50, parent = pop1) %>%
    resize(time = 800, N = 10, how = "step")

  model <- compile_model(list(pop1, pop2), generation_time = 100, simulation_length = 5000)

  tss <- slim(model, sequence_length = 1e6, recombination_rate = 1e-8)
  tsm <- msprime(model, sequence_length = 1e6, recombination_rate = 1e-8)

  expect_equal(ts_samples(tss), ts_samples(tsm))
})
