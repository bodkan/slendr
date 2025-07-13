skip_if(!check_dependencies(python = TRUE))

init_env(quiet = TRUE)

afr <- population("AFR", time = 100000, N = 3000)
ooa <- population("OOA", parent = afr, time = 60000, N = 500, remove = 23000)
ehg <- population("EHG", parent = ooa, time = 28000, N = 1000, remove = 6000)
eur <- population("EUR", parent = ehg, time = 25000, N = 2000)
ana <- population("ANA", time = 28000, N = 3000, parent = ooa, remove = 4000)
yam <- population("YAM", time = 7000, N = 500, parent = ehg, remove = 2500)
model <- compile_model(populations = list(afr, ooa, ehg, eur, ana, yam), generation_time = 30)

test_that("number of samples must be a non-zero integer larger than one", {
  msg <- "Sample counts must be non-negative, non-zero integer numbers"
  expect_error(schedule_sampling(model, times = 45000, list(afr, 10.3)), msg)
  expect_error(schedule_sampling(model, times = 45000, list(afr, -10)), msg)
  expect_error(schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, -42, "Ust_Ishim")), msg)
  expect_error(schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, -42, "Ust_Ishim")), msg)
})

check_ordering <- function(samples, direction) {
  if (direction == "backward")
    op <- `<=`
  else
    op <- `>=`

  dplyr::filter(samples, grepl("_\\d+$", name)) %>%
    split(., .$pop) %>%
    lapply(function(df) data.frame(i = as.integer(gsub(".*_(\\d+)", "\\1", df$name)),
                                   age = df$time)) %>%
    Filter(function(df) any(!is.na(df$i)), .) %>%
    lapply(function(df) {df$ordered <- all(diff(df$i) >= 0) && all(op(diff(df$age), 0)); df}) %>%
    do.call(rbind, .)
}

schedule <- schedule_sampling(model, times = 45000, list(afr, 10), list(ooa, 1, "Ust_Ishim"))
samples <- ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))
expect_true(all(check_ordering(samples, "backward")$ordered))

schedule <- schedule_sampling(model, times = c(45000, 30000), list(afr, 10), list(ooa, 1, "Ust_Ishim"))
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

#### TODO HERE

schedule <- dplyr::bind_rows(schedule_sampling(model, times = 15000, list(eur, 1), list(ehg, 10))
ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))

schedule <- schedule_sampling(model, times = 45000, list(ooa, 1, "Ust_Ishim"))
ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))

schedule <- schedule_sampling(
  model,
  times = c(0, 5000, 12000, 20000, 35000, 39000, 43000),
  list(eur, 3), list(ehg, 1), list(yam, 1), list(ana, 3), list(ooa, 1), list(afr, 1)
)
ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))

schedule <- schedule_sampling(
  model,
  times = c(0, 5000, 12000, 20000, 35000, 39000, 43000),
  list(eur, 3), list(ehg, 1), list(yam, 1), list(ana, 3), list(ooa, 1), list(afr, 1)
) %>% rbind(schedule_sampling(model, times = 45000, list(ooa, 1, "Ust_Ishim")))
ts_samples(msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule))

ts_msprime <- msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule)
ts_msprime <- msprime(model, sequence_length = 100000, recombination_rate = 0, samples = schedule, run = FALSE)
