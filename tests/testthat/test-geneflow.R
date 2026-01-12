test_that("non-overlapping geneflow leads to error", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 10000,
                      center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 10000,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, proportion = 0.1),
               "No overlap between population ranges of pop1 and pop2 at time 1000.")
})

test_that("non-overlapping geneflow passes if the check is explicitly turned off", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 10000,
                     center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 10000,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_s3_class(
    gene_flow(from = pop1, to = pop2, start = 1000, end = 0, proportion = 0.1, overlap = FALSE),
    "data.frame"
  )
})

test_that("populations must be present within the given gene-flow time window (spatial)", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 100,
                      center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 100,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, proportion = 0.1),
               "Both .* and .* must be already present within the gene-flow window \\d+-\\d+")
})

test_that("populations must be present within the given gene-flow time window (non-spatial)", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 100)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, proportion = 0.1),
               "Both .* and .* must be already present within the gene-flow window \\d+-\\d+")
})

test_that("gene-flow rate must be a value between 0 and 1", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 100)

  error_msg <- "The total amount of ancestry of the `to` population coming\nfrom the `from` population must be between 0 and 1"
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, proportion = -0.1), error_msg)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, proportion = 25), error_msg)
})

test_that("populations must be already created for a gene flow to happen (forward model)", {
  skip_if(!check_dependencies(python = TRUE))
  init_env(quiet = TRUE)

  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 200, parent = pop1)

  error_msg <- "^Both .* and .* must be already present within the gene-flow window .*-.*$"
  # neither population missing:
  expect_error(gene_flow(from = pop1, to = pop2, start = 50, end = 80, proportion = 0.1), error_msg)
  # pop1 present, pop2 not created yet:
  expect_error(gene_flow(from = pop1, to = pop2, start = 50, end = 130, proportion = 0.1), error_msg)
  expect_error(gene_flow(from = pop1, to = pop2, start = 200, end = 230, proportion = 0.1), error_msg)
  # (both present if gene flow starts one generation later)
  expect_silent(gene_flow(from = pop1, to = pop2, start = 201, end = 230, proportion = 0.1))
  # pop1 not yet created
  expect_error(gene_flow(from = pop1, to = pop2, start = 100, end = 230, proportion = 0.1), error_msg)

  # full run of a scenario which should pass gene_flow(), compile_model() and create a tree sequence
  expect_silent({
    gf <- gene_flow(from = pop1, to = pop2, start = 201, end = 230, proportion = 0.1)
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
  anc <- population("ancestor", time = 9e6, remove=6e6, N=100)
  popA <- population("A", time = 8e6, parent=anc, N=100)
  popB <- population("B", time = 7e6, parent=anc, N=100)

  # this used to say: "Both A and B must be present within the gene-flow window 4e+06-4000005"
  expect_error(
    gene_flow(from = popA, to = popB, start = 4e6, end = (4e6+5), proportion = 0.3),
    "Inconsistent time direction implied by populations and the gene flow event"
  )

  # this used to say: "Specified times are not consistent with the assumed direction of time
  # (gene flow A -> B in the time window 8e+06-7999995)"
  expect_error(
    gene_flow(from = popA, to = popB, start = 8e6, end = (8e6-5), proportion = 0.3),
    "Both A and B must be already present within the gene-flow window .*-.*"
  )
})

test_that("gene flow must take longer than a generation (forward models)", {
  p1 <- population("p1", time = 1, N = 100)
  p2 <- population("p2", time = 100, N = 100, parent = p1)
  p3 <- population("p3", time = 200, N = 100, parent = p2)

  # gene flow before the existence of either population
  expect_error(
    gf <- gene_flow(from = p2, to = p3, proportion = 0.5, start = 10, end = 80),
    "Both p2 and p3 must be already present within the gene-flow window 10-80"
  )
  # gene flow before the existence of the target population (off by 1 "year")
  expect_error(
    gf <- gene_flow(from = p2, to = p3, proportion = 0.5, start = 200, end = 250),
    "Both p2 and p3 must be already present within the gene-flow window 200-250"
  )
  # shifting the start by 1 "year" fixes the error
  expect_s3_class(
    gf <- gene_flow(from = p2, to = p3, proportion = 0.5, start = 201, end = 250),
    "data.frame"
  )

  # problem: gene flow can be shorter than the span of a generation

  # single invalid gene flow
  gf <- gene_flow(from = p3, to = p1, proportion = 0.5, start = 221, end = 230)
  expect_error(
    model <- compile_model(list(p1, p2, p3), generation_time = 20, gene_flow = gf, simulation_length = 1000),
    "Gene flows cannot be shorter than the generation time of 20"
  )

  # multiple invalid gene flows
  gf <- list(
    gene_flow(from = p2, to = p3, proportion = 0.5, start = 201, end = 210),
    gene_flow(from = p3, to = p1, proportion = 0.5, start = 221, end = 230)
  )

  expect_error(
    model <- compile_model(list(p1, p2, p3), generation_time = 20,
                           gene_flow = gf, simulation_length = 1000),
    "Gene flows cannot be shorter than the generation time of 20"
  )

  # single invalid gene flows among multiple
  gf <- list(
    gene_flow(from = p2, to = p3, proportion = 0.5, start = 201, end = 250),
    gene_flow(from = p3, to = p1, proportion = 0.5, start = 221, end = 230)
  )

  expect_error(
    model <- compile_model(list(p1, p2, p3), generation_time = 20,
                           gene_flow = gf, simulation_length = 1000),
    "Gene flows cannot be shorter than the generation time of 20"
  )

  # properly timed gene flows work OK
  t <- 20
  gf <- list(
    gene_flow(from = p2, to = p3, proportion = 0.5, start = 300, end = 300 + t),
    gene_flow(from = p3, to = p1, proportion = 0.5, start = 500, end = 500 + t)
  )
  expect_s3_class(
    model <- compile_model(list(p1, p2, p3), generation_time = t,
                           gene_flow = gf, simulation_length = 1000),
    "slendr_model"
  )
})

test_that("gene flow must take longer than a generation (backward models)", {
  p1 <- population("p1", time = 1000, N = 100)
  p2 <- population("p2", time = 800, N = 100, parent = p1)
  p3 <- population("p3", time = 600, N = 100, parent = p2)

  # gene flow before the existence of either population
  expect_error(
    gf <- gene_flow(from = p2, to = p3, proportion = 0.5, start = 1000, end = 900),
    "Both p2 and p3 must be already present within the gene-flow window 1000-900",
  )
  # gene flow before the existence of the target population (off by 1 "year")
  expect_error(
    gf <- gene_flow(from = p2, to = p3, proportion = 0.5, start = 600, end = 500),
    "Both p2 and p3 must be already present within the gene-flow window 600-500"
  )
  # shifting the start by 1 "year" fixes the error
  expect_s3_class(
    gf <- gene_flow(from = p2, to = p3, proportion = 0.5, start = 599, end = 500),
    "data.frame"
  )

  # problem: gene flow can be shorter than the span of a generation

  # single invalid gene flow
  gf <- gene_flow(from = p3, to = p1, proportion = 0.5, start = 500, end = 490)
  expect_error(
    model <- compile_model(list(p1, p2, p3), generation_time = 20, gene_flow = gf, simulation_length = 1000),
    "Gene flows cannot be shorter than the generation time of 20"
  )

  # multiple invalid gene flows
  gf <- list(
    gene_flow(from = p2, to = p3, proportion = 0.5, start = 500, end = 490),
    gene_flow(from = p3, to = p1, proportion = 0.5, start = 100, end = 90)
  )
  expect_error(
    model <- compile_model(list(p1, p2, p3), generation_time = 20,
                           gene_flow = gf, simulation_length = 1000),
    "Gene flows cannot be shorter than the generation time of 20"
  )

  # single invalid gene flows among multiple
  gf <- list(
    gene_flow(from = p2, to = p3, proportion = 0.5, start = 500, end = 490),
    gene_flow(from = p3, to = p1, proportion = 0.5, start = 100, end = 90)
  )
  expect_error(
    model <- compile_model(list(p1, p2, p3), generation_time = 20,
                           gene_flow = gf, simulation_length = 1000),
    "Gene flows cannot be shorter than the generation time of 20"
  )

  # properly timed gene flows work OK
  t <- 20
  gf <- list(
    gene_flow(from = p2, to = p3, proportion = 0.5, start = 500, end = 500 - t),
    gene_flow(from = p3, to = p1, proportion = 0.5, start = 100, end = 100 - t)
  )
  expect_s3_class(
    model <- compile_model(list(p1, p2, p3), generation_time = t,
                           gene_flow = gf, simulation_length = 1000),
    "slendr_model"
  )
})
