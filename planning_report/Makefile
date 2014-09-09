make:
	emacs -q --batch \
		--eval "(setq package-load-list '((org t)))" \
		--eval "(package-initialize)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f\" \"biber %b.bcf\" \"pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f\"))" \
		--visit planning_report.org --funcall org-latex-export-to-pdf
clean:
	rm -f *.{aux,bbl,bcf,blg,dvi,fdb_latexmk,fls,log,out,pdf,pyg,run.xml,tex,tex.blg,toc}
	rm -f planning_report.tex~
