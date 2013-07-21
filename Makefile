PANDOC_OPTS=--bibliography=library.bib

all : submission.pdf

%.pdf : %.mkd
	pandoc -o $@ $< --latex-engine=lualatex $(PANDOC_OPTS)
