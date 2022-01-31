.PHONY: build vignettes

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/slendr_$(version).tar.gz

website: README.md
	rm -rf docs/ man/; mkdir -p man/figures
	cp vignettes/images/shiny_graph.jpg man/figures/
	cp vignettes/images/shiny_maps.jpg man/figures/
	R -e 'devtools::document(); options(pkgdown.internet = FALSE); pkgdown::build_site()'
	git restore docs/CNAME
	git restore man/figures/logo.png docs/reference/figures/logo.png docs/logo.png

logo:
	R -e 'source("logo.R")'

build: $(pkg)

check: $(pkg)
	cd build; R CMD check --as-cran $(notdir $<)

winrel: README.md
	R -e 'devtools::check_win_release()'

windev: README.md
	R -e 'devtools::check_win_devel()'

winold: README.md
	R -e 'devtools::check_win_oldrelease()'

$(pkg): README.md
	R -e 'devtools::document()'
	rm -rf build; mkdir build; cd build; \
	R CMD build --log ../../slendr

README.md: README.Rmd
	R -e 'devtools::install()'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'

clean:
	rm -rf build README.md
