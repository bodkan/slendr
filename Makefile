.PHONY: build vignettes

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/slendr_$(version).tar.gz

website:
	R -e 'devtools::install()'
	rm -rf docs/ man/; mkdir -p man/figures
	cp vignettes/images/shiny_graph.jpg man/figures/
	cp vignettes/images/shiny_maps.jpg man/figures/
	R -e 'devtools::document(); knitr::knit("README.Rmd", output = "README.md"); options(pkgdown.internet = FALSE); pkgdown::build_site()'
	git restore docs/CNAME
	git restore man/figures/logo.png docs/reference/figures/logo.png docs/logo.png

logo:
	R -e 'source("logo.R")'

build: $(pkg)

check: $(pkg)
	cd build; R CMD check --as-cran $(notdir $<)

$(pkg):
	R -e 'knitr::knit("README.Rmd", output = "README.md")'
	rm -rf build; mkdir build; cd build; \
	R CMD build --log ../../slendr

clean:
	rm -rf build
