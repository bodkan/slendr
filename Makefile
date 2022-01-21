website:
	rm -rf docs/ man/; mkdir -p man/figures
	cp vignettes/figures/shiny_graph.png man/figures/
	cp vignettes/figures/shiny_maps.png man/figures/
	R -e 'devtools::document(); source("logo.R"); knitr::knit("README.Rmd", output = "README.md"); options(pkgdown.internet = FALSE); pkgdown::build_site();'

vignettes:
	R -e 'knitr::knit("README.Rmd.orig", output = "README.md")'
	cd vignettes/; \
	for rmd in *.Rmd.orig; do \
		R -e "knitr::knit(\"$$rmd\", \"$${rmd%.orig}\")"; \
	done

restore:
	git restore man/figures/logo.png docs/reference/figures/logo.png docs/logo.png
	git restore docs/CNAME