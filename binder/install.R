devtools::install_github("bodkan/slendr") # 2022-02-16 12:12

# create the Miniconda environment so that users don't have to wait
# for setup_env() in the vignette examples
R -e 'slendr::setup_env()'
