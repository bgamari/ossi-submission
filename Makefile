PANDOC_OPTS=--bibliography=library.bib -V links-as-notes -V geometry:margin=1in --template=template.tex

all : library.bib submission.pdf

submission.pdf : fret-setup.pdf ecosystem.pdf rna-fret-hist.pdf

%.tex : %.mkd
	pandoc -o $@ $< --standalone $(PANDOC_OPTS)

%.pdf : %.mkd
	pandoc -o $@ $< --latex-engine=lualatex $(PANDOC_OPTS)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

library.bib : /home/ben/lori/papers/library.bib
	cp $< $@
	git commit -m "Update library.bib" $@
