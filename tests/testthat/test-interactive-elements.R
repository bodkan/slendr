times <- 1:10

test_that("selecting time points lower than minimum is prevented", {
  expect_true(get_time_point(times, min(times), "next") == min(times))
})

test_that("selecting time points higher than maximum is prevented", {
  expect_true(get_time_point(times, max(times), "previous") == max(times))
})
