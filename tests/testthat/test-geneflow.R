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

test_that("populations must be present for them to mix", {
  map <- readRDS("map.rds")
  pop1 <- population("pop1", N = 100, time = 100,
                      center = c(0, 10), radius = 1, map = map, intersect = FALSE)
  pop2 <- population("pop2", N = 100, time = 100,
                     center = c(0, -10), radius = 1, map = map, intersect = FALSE)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1),
               "Specified times are not consistent with the assumed direction", fixed = TRUE)
})

test_that("populations must be present for them to mix (non-spatial models)", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 100)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 0.1),
               "Specified times are not consistent with the assumed direction", fixed = TRUE)
})

test_that("gene-flow rate must be a value between 0 and 1", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 100)

  error_msg <- "Gene-flow rate must be a numeric value between 0 and 1"
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = -0.1), error_msg)
  expect_error(gene_flow(from = pop1, to = pop2, start = 1000, end = 0, rate = 25), error_msg)
})

test_that("populations must be already created for a gene flow to happen (forward model)", {
  pop1 <- population("pop1", N = 100, time = 100)
  pop2 <- population("pop2", N = 100, time = 200, parent = pop1)

  expect_error(
    gene_flow(from = pop1, to = pop2, start = 50, end = 80, rate = 0.1),
    "^Both .* and .* must be present within the gene-flow window .*-.*$"
  )
  expect_error(
    gene_flow(from = pop1, to = pop2, start = 50, end = 130, rate = 0.1),
    "Specified times are not consistent with the assumed direction"
  )

  expect_silent(gene_flow(from = pop1, to = pop2, start = 200, end = 230, rate = 0.1))
  expect_silent(gene_flow(from = pop1, to = pop2, start = 210, end = 230, rate = 0.1))
})
