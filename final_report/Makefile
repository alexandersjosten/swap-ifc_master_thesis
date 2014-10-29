all:
	pdflatex thesis.tex
	bibtex thesis.aux
	pdflatex thesis.tex
	pdflatex thesis.tex
clean:
	rm -f *.{aux,lof,log,out,toc,lot,bbl,blg,pdf}
	mv images/frontpage.pdf ./frontpage.pdf
	rm -f images/*.pdf
	mv ./frontpage.pdf images/frontpage.pdf
