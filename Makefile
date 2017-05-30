all: README.html

README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd')"

README.html: README.md references.bib
	pandoc -o README.html README.md
