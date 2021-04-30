website:
	R -e 'knitr::knit("README.Rmd", output="README.md")'
	R -e 'devtools::document()'
	R -e 'pkgdown::build_site()'

logo:
	Rscript logo.R
