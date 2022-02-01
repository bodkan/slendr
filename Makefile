.PHONY: build vignettes

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/slendr_$(version).tar.gz

website: README.md logo.png
	R -e 'devtools::document(); options(pkgdown.internet = FALSE); pkgdown::build_site()'

build: $(pkg)

check: $(pkg)
	cd build; R CMD check --as-cran $(notdir $<)

winrel: README.md
	R -e 'devtools::check_win_release()'

windev: README.md
	R -e 'devtools::check_win_devel()'

winold: README.md
	R -e 'devtools::check_win_oldrelease()'

rhub: $(pkg)
	R -e 'rhub::check_for_cran("$<")'

$(pkg): README.md logo.png
	R -e 'devtools::document()'
	mkdir -p build; cd build; R CMD build --log ../../slendr

README.md: README.Rmd
	R -e 'devtools::install()'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'

logo.png: logo.R
	R -e 'source("logo.R")'

clean:
	rm -rf build
