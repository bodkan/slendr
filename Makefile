website: README.md
	R -e 'devtools::document()'
	R -e 'pkgdown::build_site()'

README.md: README.Rmd
	R -e 'knitr::knit("$<", output="$@")'
