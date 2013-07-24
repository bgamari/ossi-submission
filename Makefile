PANDOC_OPTS=--bibliography=library.bib -V links-as-notes

all : submission.pdf

submission.pdf : fret-setup.pdf fret_processes.pdf ecosystem.pdf

%.pdf : %.mkd
	pandoc -o $@ $< --latex-engine=lualatex $(PANDOC_OPTS)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
