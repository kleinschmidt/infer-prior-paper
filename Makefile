.PHONY: all supp

all: infer-prior.html infer-prior.pdf
supp: infer-prior-supp.html infer-prior-supp.pdf

%.html: %.Rmd
	Rscript -e "rmarkdown::render('$<', output_format='html_document', output_file='$@', clean=FALSE)"

%.pdf: %.Rmd apa6.template.tex
	Rscript -e "rmarkdown::render('$<', output_format='pdf_document', output_file='$@', clean=FALSE)"
