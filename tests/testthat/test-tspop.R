skip_if(!is_slendr_env_present())
init_env(quiet = TRUE)

anc_all <- population("ancestor_all", time = 700e3, N = 10000, remove = 640e3)
afr <- population("AFR", parent = anc_all, time = 650e3, N = 10000)
anc_arch <- population("ancestor_archaics", parent = anc_all, time = 650e3, N = 10000, remove = 390e3)
nea <- population("NEA", parent = anc_arch, time = 400e3, N = 2000, remove = 30e3)
den <- population("DEN", parent = anc_arch, time = 400e3, N = 2000, remove = 30e3)
nonafr <- population("nonAFR", parent = afr, time = 100e3, N = 3000, remove = 39e3)
eur <- population("EUR", parent = nonafr, time = 45e3, N = 5000)
pap <- population("PAP", parent = nonafr, time = 45e3, N = 5000)

gf <- list(
  gene_flow(from = nea, to = nonafr, rate = 0.03, start = 55000, end = 50000),
  gene_flow(from = den, to = pap, rate = 0.07, start = 35000, end = 30000)
)

model <- compile_model(
  populations = list(anc_all, afr, anc_arch, nea, den, nonafr, eur, pap),
  gene_flow = gf,
  generation_time = 30
)

# plot_model(
#   model, sizes = FALSE,
#   order = c("AFR", "EUR", "nonAFR", "PAP", "ancestor_all", "DEN", "ancestor_archaics", "NEA")
# )

samples <- schedule_sampling(model, times = 0, list(eur, 25), list(pap, 25))

ts <- msprime(model, sequence_length = 10e6, recombination_rate = 1e-8, samples = samples)

test_that("census time must correspond to the start of some gene-flow event", {
  expect_error(ts_tracts(ts, census = 123),
               "Census time 123 does not correspond to any gene-flow times")
})

test_that("if given, source population must correspond to the given gene-flow event", {
  expect_error(ts_tracts(ts, census = 55000, source = "nonexistent"),
               "These source\\(s\\) do not participate in the specified gene flow event: nonexistent")
})

test_that("given source population filters the admixture tracts", {
  capture.output(tracts <- ts_tracts(ts, census = 55000, source = "NEA"))
  expect_true(unique(tracts$source_pop) == "NEA")

  capture.output(tracts <- ts_tracts(ts, census = 35000, source = "DEN"))
  expect_true(unique(tracts$source_pop) == "DEN")
})
