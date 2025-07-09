.PHONY: website docs build check windev winrel winold clean

version := $(shell less DESCRIPTION | grep 'Version' | sed 's/Version: \(.*\)$$/\1/')
pkg := build/slendr_$(version).tar.gz
logo := man/figures/logo.png

docs:
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'
	R -e 'pkgdown::build_news()'
	#rm docs/reference/Rplot001.png
	#git restore docs/reference/join.html
	#git restore docs/reference/msprime.html
	#git restore docs/reference/region.html
	#git restore docs/reference/slim.html
	#git restore docs/reference/world.html
	#git restore docs/reference/expand_range-1.png

website: $(logo) README.md
	R -e 'devtools::install(upgrade = "never")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_reference()'
	R -e 'pkgdown::build_reference_index()'
	R -e 'pkgdown::build_news()'
	R -e 'pkgdown::build_site()'
	#rm docs/reference/Rplot001.png
	# discard useless updates of temporary paths, random seed values, etc.
	#git restore docs/pkgdown.yml
	#git restore docs/reference/join.html
	#git restore docs/reference/msprime.html
	#git restore docs/reference/region.html
	#git restore docs/reference/slim.html
	#git restore docs/reference/world.html
	#git restore docs/reference/expand_range-1.png
	git restore docs/reference/area-1.png

test:
	R -e 'devtools::test()'

build: $(pkg)

check: $(pkg)
	unset R_HAS_GGTREE; cd build; R CMD check --as-cran $(notdir $<)

winrel: README.md
	unset R_HAS_GGTREE; R -e 'devtools::check_win_release()'

windev: README.md
	unset R_HAS_GGTREE; R -e 'devtools::check_win_devel()'

winold: README.md
	unset R_HAS_GGTREE; R -e 'devtools::check_win_oldrelease()'

clean:
	rm -rf build

$(pkg): README.md
	R -e 'devtools::document()'
	unset R_HAS_GGTREE; mkdir -p build; cd build; R CMD build --log ../../slendr

README.md: README.Rmd $(logo)
	R -e 'devtools::install(upgrade = "never")'
	R -e 'knitr::knit("README.Rmd", output = "README.md")'

$(logo): logo.R
	Rscript logo.R

example_data:
	rm -rf inst/extdata/models/; mkdir -p inst/extdata/models/
	Rscript generate_examples.R

