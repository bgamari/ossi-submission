figures = fret1.pdf fret_processes.pdf fret-setup.pdf

talk.pdf : talk.mkd $(figures)
	pandoc -o $@ $< --latex-engine=xelatex

fret1.svg : fret.svg ExportLayers.hs
	runghc -hide-package=monads-tf ExportLayers.hs

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
