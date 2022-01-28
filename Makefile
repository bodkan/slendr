.PHONY: vignettes

website:
	R -e 'devtools::install()'
	rm -rf docs/ man/; mkdir -p man/figures
	cp vignettes/images/shiny_graph.jpg man/figures/
	cp vignettes/images/shiny_maps.jpg man/figures/
	R -e 'devtools::document(); knitr::knit("README.Rmd", output = "README.md"); options(pkgdown.internet = FALSE); pkgdown::build_site()'
	git restore docs/CNAME
	git restore man/figures/logo.png docs/reference/figures/logo.png docs/logo.png

check:
	R -e 'devools::check()'
	R -e 'devtools::check_win_devel()'
	R -e 'devtools::check_win_release()'
	R -e 'devtools::check_rhub()'

logo:
	R -e 'source("logo.R")'
