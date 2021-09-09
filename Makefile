backend:
	version=`awk '/Version/ { print $$2 }' DESCRIPTION`; \
	hash=`git log --pretty=format:'%h' -n 1`; \
	sed -i 's/__VERSION__/'"$${version}_$${hash}"'/' inst/extdata/backend.slim

restore:
	git restore man/figures/logo.png docs/reference/figures/logo.png docs/logo.png

website:
	rm -rf docs/ man/; mkdir -p man/figures
	cp vignettes/figures/shiny_graph.png man/figures/
	cp vignettes/figures/shiny_maps.png man/figures/
	R -e 'devtools::document(); source("logo.R"); knitr::knit("README.Rmd", output = "README.md"); options(pkgdown.internet = FALSE); pkgdown::build_site();'
