slides.pdf : slides.mkd figures
	pandoc -o $@ $< --latex-engine=lualatex -t beamer

slides.mkd : ExportLayers.hs $(figs)
	runghc -hide-package=monads-tf ExportLayers.hs

figs = $(wildcard *.svg)
figures : $(figs:.svg=.pdf)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

clean :
	rm -f $(addsuffix .pdf, $(fret_figures))
	rm -f $(addsuffix .svg, $(fret_figures))
	rm -f talk.pdf
