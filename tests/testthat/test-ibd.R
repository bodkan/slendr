devtools::load_all()
skip_if(!is_slendr_env_present())
init_env()

pop <- population("POP", time = 1, N = 1000)
model <- compile_model(populations = pop, generation_time = 1, simulation_length = 1000)

ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8)

test_that("aggregate ts_ibd(ts, coordinates = TRUE) matches IBD totals", {
  ibd_totals <- ts_ibd(ts, coordinates = FALSE, min_length = 5e6)
  ibd_fragments <- ts_ibd(ts, coordinates = TRUE, min_length = 5e6)

  # compute IBD totals from individual fragments manually
  ibd_totals2 <-
    dplyr::group_by(ibd_fragments, node1, node2, name1, name2, pop1, pop2) %>%
    dplyr::summarise(count = dplyr::n(), total = sum(length), .groups = "keep") %>%
    dplyr::select(count, total, dplyr::everything()) %>%
    dplyr::ungroup()

  expect_equal(ibd_totals, ibd_totals2)
})
