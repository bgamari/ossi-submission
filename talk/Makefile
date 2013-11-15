slides.pdf : slides.mkd figures
	pandoc -o $@ $< --latex-engine=xelatex -t beamer --template=./template

slides.mkd : ExportLayers.hs $(figs)
	runghc -hide-package=monads-tf ExportLayers.hs

figs = $(wildcard *.svg)
figures : $(figs:.svg=.pdf)

%.pdf : %.svg
	rsvg-convert -f pdf -o $@ $<

clean :
	rm -f $(addsuffix .pdf, $(fret_figures))
	rm -f $(addsuffix .svg, $(fret_figures))
	rm -f talk.pdf

present : slides.pdf
	hpdfp slides.pdf
