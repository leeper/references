all: README.html

README.md: README.Rmd references.bib
	Rscript -e "rmarkdown::render('README.Rmd')"

README.html: README.md
	pandoc -o README.html README.md
