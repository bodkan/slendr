test_that("load() restores the original model object", {
  world <- readRDS("world.rds")
  pop <- population("pop", parent = "ancestor", Ne = 10,
                    center = c(10, 40), radius = 100, world = world)

  model_dir <- file.path(tempdir(), "tmp-model")
  model1 <- compile(pop, model_dir = model_dir, resolution = 10, gen_time = 1)
  model2 <- load(model1$directory)

  # make sure that all components of the model list object before and after
  # serialization are equal
  components <- c("directory", "splits", "admixtures", "maps", "path", "gen_time")
  expect_true(all(sapply(components, function(i) all(model1[[i]] == model2[[i]]))))
})
