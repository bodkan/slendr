test_that("non-overlapping geneflow leads to error", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 10000,
                      center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 10000,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1),
               "No overlap between population ranges of pop1 and pop2 at time 1000.")
})

test_that("non-overlapping geneflow passes if the check is explicitly turned off", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 10000,
                     center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 10000,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_s3_class(
    gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1, overlap = FALSE),
    "data.frame"
  )
})

test_that("populations must be present within the given gene-flow time window (spatial)", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 100,
                      center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 100,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1),
               "Both .* and .* must be already present within the gene-flow window \\d+-\\d+")
})

test_that("populations must be present within the given gene-flow time window (non-spatial)", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 100)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1),
               "Both .* and .* must be already present within the gene-flow window \\d+-\\d+")
})

test_that("gene-flow rate must be a value between 0 and 1", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 100)

  error_msg <- "Gene-flow rate must be a numeric value between 0 and 1"
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = -0.1), error_msg)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 25), error_msg)
})

test_that("populations must be already created for a gene flow to happen (forward model)", {
  skip_if(!is_slendr_env_present())
  init_env(quiet = TRUE)

  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 200, parent = pop1)

  error_msg <- "^Both .* and .* must be already present within the gene-flow window .*-.*$"
  # neither population missing:
  expect_error(gene_flow(from = pop1, to = pop2, start = 50, end = 80, rate = 0.1), error_msg)
  # pop1 present, pop2 not created yet:
  expect_error(gene_flow(from = pop1, to = pop2, start = 50, end = 130, rate = 0.1), error_msg)
  expect_error(gene_flow(from = pop1, to = pop2, start = 200, end = 230, rate = 0.1), error_msg)
  # (both present if gene flow starts one generation later)
  expect_silent(gene_flow(from = pop1, to = pop2, start = 201, end = 230, rate = 0.1))
  # pop1 not yet created
  expect_error(gene_flow(from = pop1, to = pop2, start = 100, end = 230, rate = 0.1), error_msg)

  # full run of a scenario which should pass gene_flow(), compile_model() and create a tree sequence
  expect_silent({
    gf <- gene_flow(from = pop1, to = pop2, start = 201, end = 230, rate = 0.1)
    model <- compile_model(list(pop1, pop2), generation_time = 1, simulation_length = 201)
  })
  expect_s3_class(ts_msprime <- msprime(model, sequence_length = 100, recombination_rate = 0),
                  "slendr_ts")
  expect_s3_class(ts_slim <- slim(model, sequence_length = 100, recombination_rate = 0),
                  "slendr_ts")
})

# possibly reduntant given the above, but a useful test of an issue reported on Github
# (for quite a long time, gene_flow() failed correctly but with confusing error message
# due to weird code duplication inside gene_flow()) -- this test is added to check
# that the issue has been solved
# https://github.com/bodkan/slendr/issues/132
test_that("gene_flow() behaves as expected in some concrete situations", {
  anc <- population("ancestor", time = 9e6, remove=8e6, N=100)
  popA <- population("A", time = 8e6, parent=anc, N=100)
  popB <- population("B", time = 7e6, parent=anc, N=100)

  # this used to say: "Both A and B must be present within the gene-flow window 4e+06-4000005"
  expect_error(
    gene_flow(from = popA, to = popB, start = 4e6, end = (4e6+5), rate = 0.3),
    "Inconsistent time direction implied by populations and the gene flow event"
  )

  # this used to say: "Specified times are not consistent with the assumed direction of time
  # (gene flow A -> B in the time window 8e+06-7999995)"
  expect_error(
    gene_flow(from = popA, to = popB, start = 8e6, end = (8e6-5), rate = 0.3),
    "Both A and B must be already present within the gene-flow window .*-.*"
  )
})
