



ts_tajima(ts, list(afr = c("AFR_1", "AFR_2", "AFR_3"), eur = c("EUR_1", "EUR_2")))

ts_diversity(ts, sample_sets) %>% dplyr::arrange(diversity)

ts_divergence(ts, sample_sets) %>% arrange(divergence)