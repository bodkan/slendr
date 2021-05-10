times <- 1:10

test_that("selecting time points lower than minimum is prevented", {
  expect_true(get_timepoint(times, min(times), "previous") == min(times))
})

test_that("selecting time points higher than maximum is prevented", {
  expect_true(get_timepoint(times, max(times), "next") == max(times))
})
