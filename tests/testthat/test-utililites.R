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

test_that("SLiMgui is found correctly", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")
  expect_equal(get_binary("gui"), "open -a SLiMgui")
})

test_that("slim is found correctly", {
  skip_on_cran()
  skip_on_os("windows")
  expect_equal(Sys.which(get_binary("batch")),
               Sys.which("slim"))
})
