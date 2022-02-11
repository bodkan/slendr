.PHONY: build vignettes docs

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/slendr_$(version).tar.gz
logo := man/figures/logo.png

docs:
	R -e 'devtools::document(); pkgdown::build_reference(examples = FALSE)'

website:
	rm -rf docs/
	R -e 'devtools::install(upgrade = "never")'
	R -e 'source("logo.R")'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_site(examples = FALSE)'
	git restore docs/pkgdown.yml
	git restore docs/CNAME

build: $(pkg)

check: $(pkg)
	cd build; R CMD check --as-cran $(notdir $<)

winrel: README.md
	R -e 'devtools::check_win_release()'

windev: README.md
	R -e 'devtools::check_win_devel()'

winold: README.md
	R -e 'devtools::check_win_oldrelease()'

rhub: README.md
	R -e 'rhub::check_for_cran()'

$(pkg): README.md
	R -e 'devtools::document()'
	mkdir -p build; cd build; R CMD build --log ../../slendr

README.md: README.Rmd $(logo)
	R -e 'devtools::install(upgrade = "never")'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'

$(logo): logo.R
	R -e 'source("logo.R")'

clean:
	rm -rf build
