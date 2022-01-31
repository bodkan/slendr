ts <- ts_load(model)

ts <- ts_load(model, simplify = TRUE)
ts

ts_small <- ts_simplify(ts, simplify_to = c("CH_1", "NEA_1", "NEA_2", "AFR_1", "AFR_2", "EUR_20", "EUR_50"))
ts_small

ts <- ts_load(model, recapitate = TRUE, simplify = TRUE,
              recombination_rate = 1e-8, Ne = 10000)

ts_coalesced(ts)

ts <- ts_mutate(ts, mutation_rate = 1e-8, random_seed = 314159)
ts

ts <- ts_load(model, recapitate = TRUE, simplify = TRUE, random_seed = 314159,
              recombination_rate = 1e-8, Ne = 10000, mutation_rate = 1e-8)

tree <- ts_tree(ts_small, i = 1)
tree

ts_draw(tree, width = 1000, height = 600, labels = TRUE, ts = ts_small, time_scale = "rank")

# f2 is a measure of the branch length connecting A and B
ts_f2(ts, A = "EUR_1", B = "AFR_1")

# f4 is a measure of the drift shared between A and B after their split from C
ts_f3(ts, A = "EUR_1", B = "AFR_1", C = "CH_1")

# this value should be very close to zero (no introgression in Africans)
ts_f4(ts, "AFR_1", "AFR_2", "NEA_1", "CH_1", mode = "branch")

# this value should be significantly negative (many more ABBA sites
# compared to BABA site due to the introgression into Europeans)
ts_f4(ts, "AFR_1", "EUR_1", "NEA_1", "CH_1", mode = "branch")

ts_samples(ts)

ts_vcf(ts, path = file.path(tempdir(), "output.vcf.gz"))

ts_fst(ts, sample_sets = list(afr = c("AFR_1", "AFR_2", "AFR_3"), eur = c("EUR_1", "EUR_2")))

ts_tajima(ts, list(afr = c("AFR_1", "AFR_2", "AFR_3"), eur = c("EUR_1", "EUR_2")))

ts_diversity(ts, sample_sets) %>% dplyr::arrange(diversity)

ts_divergence(ts, sample_sets) %>% arrange(divergence)