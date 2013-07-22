PANDOC_OPTS=--bibliography=library.bib

all : submission.pdf

submission.pdf : fret-setup.pdf

%.pdf : %.mkd
	pandoc -o $@ $< --latex-engine=lualatex $(PANDOC_OPTS)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
