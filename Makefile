PANDOC_OPTS=--bibliography=library.bib -V links-as-notes -V geometry:margin=1in --template=template.tex

all : submission.pdf walkthrough.pdf

submission.pdf : fret-setup.pdf ecosystem.pdf rna-fret-hist.pdf
walkthrough.pdf : 

%.tex : %.mkd library.bib
	pandoc -o $@ $< --standalone $(PANDOC_OPTS)

%.pdf : %.mkd library.bib
	pandoc -o $@ $< --latex-engine=lualatex $(PANDOC_OPTS)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

library.bib : /home/ben/lori/papers/library.bib
	grep -v "url =" $< > $@
	git commit -m "Update library.bib" $@

.PHONY : package
package : submission.pdf walkthrough.pdf
	mkdir -p package
	cp submission.pdf package/gamar-long.pdf
	cp walkthrough.pdf package/gamar-walkthrough.pdf
	cp short.mkd package/gamar-short.mkd
	tar -cjf package/gamar-soft1-photon-tools.tar.bz2 photon-tools
	tar -cjf package/gamar-soft2-timetag-tools.tar.bz2 timetag-tools
	tar -cjf package/gamar-soft3-hphoton.tar.bz2 hphoton

