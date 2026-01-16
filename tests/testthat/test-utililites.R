skip_on_cran()

test_that("distances beyond the world dimension throw and error", {
  map <- readRDS("map.rds")
  xrange <- diff(sf::st_bbox(map)[c("xmin", "xmax")])
  yrange <- diff(sf::st_bbox(map)[c("ymin", "ymax")])
  error_msg <- "larger than the overall world size"
  expect_error(check_resolution(map, xrange * 10), error_msg)
  expect_error(check_resolution(map, yrange * 10), error_msg)
})

test_that("distances beyond the world dimension throw and error", {
  map <- readRDS("map.rds")
  xrange <- diff(sf::st_bbox(map)[c("xmin", "xmax")])
  yrange <- diff(sf::st_bbox(map)[c("ymin", "ymax")])
  expect_silent(check_resolution(map, xrange / 10))
  expect_silent(check_resolution(map, yrange / 10))
})

test_that("concat() function correctly collapses sample identifiers", {
  expect_equal(concat(c("A", "B")), "A+B")
  expect_equal(concat(list(c("A", "B"))), "A+B")
  expect_equal(concat(list(xyz=c("A", "B"))), "xyz")
})
