map <- readRDS("map.rds")
pop <- population("pop1", N = 100, time = 100,
                  center = c(0, 25), radius = 100000, map = map, intersect = FALSE)
model <- compile_model(
  pop, path = normalizePath(tempfile(), winslash = "/", mustWork = FALSE),
  generation_time = 1, resolution = 100000,
  competition = 100e3, mating = 100e3, dispersal = 10e3,
  direction = "backward"
)

######################################################################
# error handling
######################################################################

test_that("model object required for raster coordinate conversion", {
  coord <- c(10, 10)
  error_msg <- "Model object needs to be specified for conversion of raster coordinates"
  expect_error(reproject(coord[1], coord[2], from = "raster", to = 3035), error_msg)
  expect_error(reproject(coord[1], coord[2], from = 4326, to = "raster"), error_msg)
})

test_that("coordinates in some form must be provided", {
  error_msg <- "Coordinates for conversion are missing"
  expect_error(reproject(from = 4326, to = 3035), error_msg)
  expect_error(reproject(x = 0, from = 4326, to = 3035), error_msg)
  expect_error(reproject(y = 0, from = 4326, to = 3035), error_msg)
  df <- data.frame(x = 0, y = 0)
})

test_that("'x' and 'y' columns are present in each input data.frame", {
  df1 <- data.frame(x = c(1, 10))
  df2 <- data.frame(y = c(1, 10))
  df3 <- data.frame(a = 1, b = 2)
  error_msg <- "Columns 'x' and 'y' must be present in the input data.frame"
  expect_error(reproject(coords = df1, from = 4326, to = 3035), error_msg)
  expect_error(reproject(coords = df2, from = 4326, to = 3035), error_msg)
  expect_error(reproject(coords = df3, from = 4326, to = 3035), error_msg)
})

######################################################################
# conversion returns correct type
######################################################################

test_that("coordinate conversion returns a data frame", {
  expect_s3_class(reproject(x = c(1, 10), y = c(0, 5), from = "raster", to = "raster", model = model), "data.frame")
})

test_that("coordinates are correctly added to a given data.frame", {
  df <- data.frame(x = c(1, 10), y = c(0, 5))
  new_df <- reproject(coords = df, from = "raster", to = "raster", model = model, add = TRUE)
  expect_true(all(c("newx", "newy") %in% colnames(new_df)))
  expect_equal(new_df$x, new_df$newx)
  expect_equal(new_df$y, new_df$newy)
})

test_that("coordinates are correctly added to a given data.frame", {
  expect_error(reproject(x = 0, y = 0, from = 4326, to = 3035, add = TRUE),
               "Converted coordinates can only be added to a provided data.frame")
})

######################################################################
# conversion invariances
######################################################################

test_that("raster coordinates project onto themselves", {
  coord <- data.frame(x = 0, y = 0)
  new_coord <- reproject(coords = coord, from = "raster", to = "raster", model = model, output_prefix = "")
  expect_equal(coord, new_coord)
})

test_that("longitude-latitude coordinates project onto themselves", {
  coord <- c(0, 0)
  new_coord <- reproject(coord[1], coord[2], from = 4326, to = 4326, output_prefix = "")
  expect_equal(coord, as.vector(unlist(new_coord)))
})

test_that("projected coordinates project onto themselves", {
  coord <- c(0, 0)
  new_coord <- reproject(coord[1], coord[2], from = 3035, to = 3035)
  expect_equal(coord, as.vector(unlist(new_coord)))
})

test_that("geographic coordinates project onto projected coordinates", {
  coord <- c(0, 0)
  new_coord <- reproject(coord[1], coord[2], from = 4326, to = 3035, output_prefix = "")
  orig_coord <- reproject(new_coord[1], new_coord[2], from = 3035, to = 4326, output_prefix = "")
  expect_equal(coord, as.vector(unlist(orig_coord)))
})

test_that("projected coordinates project onto geographic coordinates", {
  coord <- c(1000000, 1000000)
  new_coord <- reproject(coord[1], coord[2], from = 3035, to = 4326)
  orig_coord <- reproject(new_coord[1], new_coord[2], from = 4326, to = 3035)
  expect_equal(coord, as.vector(unlist(orig_coord)))
})
