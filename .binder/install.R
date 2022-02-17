# 2022-02-16 13:32

devtools::install_github("bodkan/slendr")

# create the Miniconda environment so that users don't have to wait
# for setup_env() in the vignette examples
# reticulate::conda_create(
#   packages = c("msprime=1.1.0", "tskit=0.4.1", "pyslim=0.700", "pandas=1.3.5"),
#   envname = "automatic_slendr_python_env"
# )
slendr::setup_env(agree = TRUE)
