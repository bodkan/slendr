skip_on_cran()
skip_if(!check_dependencies(python = TRUE))

init_env(quiet = TRUE)

afr <- population("AFR", time = 100000, N = 30)
ooa <- population("OOA", parent = afr, time = 60000, N = 5, remove = 23000)
ehg <- population("EHG", parent = ooa, time = 28000, N = 10, remove = 6000)
eur <- population("EUR", parent = ehg, time = 25000, N = 20)
ana <- population("ANA", time = 28000, N = 30, parent = ooa, remove = 4000)
yam <- population("YAM", time = 7000, N = 5, parent = ehg, remove = 2500)
model <- compile_model(populations = list(afr, ooa, ehg, eur, ana, yam), generation_time = 30)

test_that("number of samples must be a non-zero integer larger than one", {
  msg <- "Sample counts must be non-negative, non-zero integer numbers"
  expect_error(schedule_sampling(model, times = 45000, list(afr, 10.3)), msg)
  expect_error(schedule_sampling(model, times = 45000, list(afr, -10)), msg)
  expect_error(schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, -42, "Ust_Ishim")), msg)
  expect_error(schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, -42, "Ust_Ishim")), msg)
})

test_that("named samples are enforced to be unique", {
  expect_error(schedule_sampling(model, times = c(45000, 30000), list(afr, 10), list(ooa, 1, "Ust_Ishim")),
               "Named samples must be unique. The following sample names are duplicated:\nUst_Ishim")
})

check_ordering <- function(samples, direction) {
  if (direction == "backward")
    op <- `<=`
  else
    op <- `>=`

  # dplyr::filter(samples, grepl("_\\d+$", name)) %>%
  samples %>%
    dplyr::group_by(pop) %>%
    dplyr::mutate(id = paste0(pop, "-", 1:dplyr::n())) %>%
    split(., .$pop) %>%
    lapply(function(df) data.frame(name = df$name,
                                   i = as.integer(gsub(".*-(\\d+)", "\\1", df$id)),
                                   age = df$time)) %>%
    Filter(function(df) any(!is.na(df$i)), .) %>%
    lapply(function(df) {df$ordered <- all(diff(df$i) >= 0) && all(op(diff(df$age), 0)); df}) %>%
    do.call(rbind, .)
}

test_that("temporal ordering of samples remains consistent with the schedule (msprime)", {
  schedule <- schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, 1, "Ust_Ishim"))
  samples <- ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  expect_true(all(check_ordering(samples, "backward")$ordered))

  schedule <- rbind(
    schedule_sampling(model, times = c(45000, 30000), list(afr, 10), list(ooa, 1)),
    schedule_sampling(model, times = 40000, list(ooa, 1, "Ust_Ishim"))
  )
  samples <- ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  expect_true(all(check_ordering(samples, "backward")$ordered))

  schedule <- dplyr::bind_rows(
    schedule_sampling(model, times = 10000, list(afr, 10)),
    schedule_sampling(model, times = 30000, list(afr, 30)),
    schedule_sampling(model, times = 40000, list(afr, 40)),
    schedule_sampling(model, times = 5000, list(afr, 5)),
    schedule_sampling(model, times = 5000, list(eur, 5)),
    schedule_sampling(model, times = 1000, list(eur, 1)),
    schedule_sampling(model, times = 3000, list(eur, 3)),
    schedule_sampling(model, times = 2000, list(eur, 2))
  )
  samples <- ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  expect_true(all(check_ordering(samples, "backward")$ordered))

  # the above tests were testing consistency of times -- here let's check 1-to-1 correspondence
  schedule <- dplyr::bind_rows(
    schedule_sampling(model, times = 10000, list(afr, 1)),
    schedule_sampling(model, times = 30000, list(afr, 1)),
    schedule_sampling(model, times = 40000, list(afr, 1)),
    schedule_sampling(model, times = 5000, list(afr, 1)),

    schedule_sampling(model, times = 35000, list(ooa, 1)),
    schedule_sampling(model, times = 41000, list(ooa, 1)),
    schedule_sampling(model, times = 40000, list(ooa, 1)),

    schedule_sampling(model, times = 5000, list(eur, 1)),
    schedule_sampling(model, times = 1000, list(eur, 1)),
    schedule_sampling(model, times = 3000, list(eur, 1)),
    schedule_sampling(model, times = 2000, list(eur, 1)),

    schedule_sampling(model, times = 10000, list(ehg, 1)),
    schedule_sampling(model, times = 18000, list(ehg, 1)),
    schedule_sampling(model, times = 17000, list(ehg, 1)),
    schedule_sampling(model, times = 9000, list(ehg, 1)),
  )
  samples <- ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  schedule_ids <- dplyr::arrange(schedule, time) %>% dplyr::group_by(pop) %>% dplyr::mutate(id = paste0(pop, "-", time, "-", 1:dplyr::n())) %>% .$id
  samples_ids <- dplyr::arrange(samples, time) %>% dplyr::group_by(pop) %>% dplyr::mutate(id = paste0(pop, "-", time, "-", 1:dplyr::n())) %>% .$id
  expect_equal(schedule_ids, samples_ids)
})

test_that("temporal ordering of samples remains consistent with the schedule (SLiM)", {
  schedule <- schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, 1, "Ust_Ishim"))
  ts <- slim(model, sequence_length = 100000, recombination_rate = 0, samples = schedule)
  samples <- ts_samples(ts)
  expect_true(all(check_ordering(samples, "backward")$ordered))

  schedule <- rbind(
    schedule_sampling(model, times = c(45000, 30000), list(afr, 10), list(ooa, 1)),
    schedule_sampling(model, times = 40000, list(ooa, 1, "Ust_Ishim"))
  )
  samples <- ts_samples(slim(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  expect_true(all(check_ordering(samples, "backward")$ordered))

  schedule <- dplyr::bind_rows(
    schedule_sampling(model, times = 10000, list(afr, 10)),
    schedule_sampling(model, times = 30000, list(afr, 30)),
    schedule_sampling(model, times = 40000, list(afr, 40)),
    schedule_sampling(model, times = 5000, list(afr, 5)),
    schedule_sampling(model, times = 5000, list(eur, 5)),
    schedule_sampling(model, times = 1000, list(eur, 1)),
    schedule_sampling(model, times = 3000, list(eur, 3)),
    schedule_sampling(model, times = 2000, list(eur, 2))
  )
  samples <- ts_samples(slim(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  expect_true(all(check_ordering(samples, "backward")$ordered))

  # the above tests were testing consistency of times -- here let's check 1-to-1 correspondence
  schedule <- dplyr::bind_rows(
    schedule_sampling(model, times = 10000, list(afr, 1)),
    schedule_sampling(model, times = 30000, list(afr, 1)),
    schedule_sampling(model, times = 40000, list(afr, 1)),
    schedule_sampling(model, times = 5000, list(afr, 1)),

    schedule_sampling(model, times = 35000, list(ooa, 1)),
    schedule_sampling(model, times = 41000, list(ooa, 1)),
    schedule_sampling(model, times = 40000, list(ooa, 1)),

    schedule_sampling(model, times = 5000, list(eur, 1)),
    schedule_sampling(model, times = 1000, list(eur, 1)),
    schedule_sampling(model, times = 3000, list(eur, 1)),
    schedule_sampling(model, times = 2000, list(eur, 1)),

    schedule_sampling(model, times = 10000, list(ehg, 1)),
    schedule_sampling(model, times = 18000, list(ehg, 1)),
    schedule_sampling(model, times = 17000, list(ehg, 1)),
    schedule_sampling(model, times = 9000, list(ehg, 1)),
  )
  samples <- ts_samples(slim(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
  schedule_ids <- dplyr::arrange(schedule, time) %>% dplyr::group_by(pop) %>% dplyr::mutate(id = paste0(pop, "-", time, "-", 1:dplyr::n())) %>% .$id
  samples_ids <- dplyr::arrange(samples, time) %>% dplyr::group_by(pop) %>% dplyr::mutate(id = paste0(pop, "-", time, "-", 1:dplyr::n())) %>% .$id
  expect_equal(schedule_ids, samples_ids)
})
