map <- readRDS("map.rds")

pop <- population("pop", time = 1000, N = 10, map = map, center = c(0, 40), radius = 500e3) %>%
  move(trajectory = c(10, 10), start = 900, end = 700, snapshots = 3)

test_that("temporal consistency of interaction parameter changes is enforced", {
  expect_error(dispersal(pop, time = 950, competition_dist = 100),
               "The new event (.*) pre-dates the last specified active event (.*)")
  expect_silent(dispersal(pop, time = 50, competition_dist = 100))
})

test_that("at least one interaction parameter is specified", {
  expect_error(dispersal(pop, time = 1000),
               "At least one spatial interaction parameter must be specified")
})

test_that("interaction parameter must be positive, non-zero values", {
  msg <- "Spatial interaction parameters can only have positive"
  expect_error(dispersal(pop, time = 1000, competition_dist = -100), msg)
  expect_error(dispersal(pop, time = 1000, mate_dist = -100), msg)
  expect_error(dispersal(pop, time = 1000, dispersal_dist = -100), msg)
})

test_that("interaction parameter change is correctly recorded", {
  x1 <- dispersal(pop, time = 100, competition_dist = 100)
  x2 <- dispersal(pop, time = 100, mate_dist = 50)
  x3 <- dispersal(pop, time = 100, dispersal_dist = 20)
  x4 <- dispersal(pop, time = 100, competition_dist = 50, dispersal_dist = 10)

  hist1 <- attr(x1, "history") %>% .[[length(.)]]
  expect_true(hist1$pop == pop$pop[1])
  expect_true(hist1$time == 100)
  expect_true(hist1$event == "dispersal")
  expect_true(hist1$competition_dist == 100)
  expect_true(is.na(hist1$mate_dist))
  expect_true(is.na(hist1$dispersal_dist))

  hist2 <- attr(x2, "history") %>% .[[length(.)]]
  expect_true(hist2$mate_dist == 50)

  hist3 <- attr(x3, "history") %>% .[[length(.)]]
  expect_true(hist3$dispersal_dist == 20)

  hist4 <- attr(x4, "history") %>% .[[length(.)]]
  expect_true(hist4$competition_dist == 50 && hist4$dispersal_dist == 10)
})

test_that("SLiM dispersals match expectations laid by R distributions", {
  skip_if(!env_present("automatic_slendr_python_env"));

  seed <- 42
  set.seed(seed)

  map <- world(xrange = c(0, 100), yrange = c(0, 100), landscape = "blank")

  slim_sim <- function(dispersal_fun, dispersal_dist, seed) {

    pop <- population("pop", time = 1, N = 3000, map = map, center = c(50, 50), radius = 0.5,
                       dispersal_dist = 0.1) %>%
      boundary(time = 2, center = c(50, 50), radius = 50) %>%
      dispersal(time = 2, dispersal_dist = dispersal_dist, dispersal_fun = dispersal_fun)

    model <- compile(
      pop, file.path(tempdir(), paste0("model_", dispersal_fun)),
      generation_time = 1, competition_dist = 0, mate_dist = 1,
      sim_length = 2, resolution = 0.1, overwrite = TRUE
    )

    slim(model, sequence_length = 1, recombination_rate = 0, method = "batch",
         save_locations = TRUE, max_attempts = 1, verbose = FALSE, random_seed = seed)

    locations <- readr::read_tsv(file.path(model$path, "output_ind_locations.tsv.gz"),
                                 show_col_types = FALSE, progress = FALSE) %>%
      reproject(coords = ., from = "raster", to = "world", model = model, add = TRUE) %>%
      dplyr::filter(gen == 0) %>%
      dplyr::mutate(distance = sqrt((newx - 50)^2 + (newy - 50)^2)) %>%
      dplyr::mutate(fun = dispersal_fun)

    locations
  }

  normal <- slim_sim("normal", 10, seed)
  uniform <- slim_sim("uniform", 10, seed)
  cauchy <- slim_sim("cauchy", 10, seed)
  exp <- slim_sim("exponential", 10, seed)

  slim_distances <- rbind(normal, uniform, cauchy, exp) %>%
    dplyr::select(distance, fun) %>%
    dplyr::mutate(source = "SLiM")

  r_sim <- function(param, fun) {
    if (fun == "rnorm")
      distance <- rnorm(1, mean = 0, sd = param)
    else if (fun == "runif")
      distance <- runif(1, min = 0, max = param)
    else if (fun == "rcauchy")
      distance <- rcauchy(1, location = 0, scale = param)
    else if (fun == "rexp")
      distance <- rexp(1, rate = 1 / param)
    else
      stop("Unknown distribution function", fun, call. = FALSE)
    angle <- runif(1, min = 0, max = 2 * pi);
    x <- distance * cos(angle);
    y <- distance * sin(angle);
    c(x, y);
  }

  n <- 10000
  r_distances <- dplyr::tibble(
    distance = c(
      sqrt(colSums(replicate(n, r_sim(10, "rnorm"))^2)),
      sqrt(colSums(replicate(n, r_sim(10, "runif"))^2)),
      sqrt(colSums(replicate(n, r_sim(10, "rcauchy"))^2)),
      sqrt(colSums(replicate(n, r_sim(10, "rexp"))^2))
    ),
    fun = c(rep("normal", n), rep("uniform", n), rep("cauchy", n), rep("exponential", n)),
    source = "R"
  ) %>%
    dplyr::filter(distance <= 50)

  distances <- rbind(slim_distances, r_distances)

  # p <- ggplot2::ggplot(distances, aes(distance, color = source)) +
  #   geom_density() +
  #   coord_cartesian(xlim = c(0, 50)) +
  #   facet_wrap(~ fun, scales = "free") +
  #   guides(color = guide_legend("simulation"))
  #
  # original_png <- "distances.png"
  # ggsave(original_png, p, width = 8, height = 5)

  # compare the SLiM dispersal distributions to the distributions randomly
  # sampled in R using the Kolmogorov-Smirnov test
  expect_true(ks.test(
    slim_distances[slim_distances$fun == "normal", ]$distance,
    r_distances[r_distances$fun == "normal", ]$distance
  )$p.value > 0.05)
  expect_true(ks.test(
    slim_distances[slim_distances$fun == "uniform", ]$distance,
    r_distances[r_distances$fun == "uniform", ]$distance
  )$p.value > 0.05)
  expect_true(ks.test(
    slim_distances[slim_distances$fun == "cauchy", ]$distance,
    r_distances[r_distances$fun == "cauchy", ]$distance
  )$p.value > 0.05)
  expect_true(ks.test(
    slim_distances[slim_distances$fun == "exponential", ]$distance,
    r_distances[r_distances$fun == "exponential", ]$distance
  )$p.value > 0.05)

  # decrease the gigantic table to make the package smaller overall
  set.seed(42)
  distances <- distances[sort(sample(1:nrow(distances), size = 5000)), ]

  current_tsv <- paste0(tempfile(), ".tsv.gz")
  readr::write_tsv(distances, current_tsv, progress = FALSE)
  original_tsv <- "distances.tsv.gz"
  # readr::write_tsv(distances, original_tsv, progress = FALSE)
  orig_distances <- readr::read_tsv(original_tsv, show_col_types = FALSE, progress = FALSE)

  # make sure that the current distance distribution matches the original one
  expect_equal(distances, orig_distances, tolerance = 1e-15)
})
