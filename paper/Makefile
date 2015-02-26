CURRENT=paper-hosc

WGETMATTMIGHTBIB=wget -O mattmight.bib "http://www.citeulike.org/bibtex/user/mattmight?fieldmap=posted-at:date-added&do_username_prefix=1&key_type=4"
WGETMACROS=wget -O matt-might-macros.tex http://matt.might.net/share/tex/matt-might-macros.tex

default: $(CURRENT).pdf

.PHONY: wc
wc:
	@wc -w abstract.tex
	@wc -w content.tex

refresh: getbib getmacros

lrefresh:
	cp -v ~/Sites/matt.might.net/trunk/share/tex/matt-might-macros.tex .

getmacros:
	$(WGETMACROS)

getbib:
	$(WGETMATTMIGHTBIB)

bibtex:
	bibtex $(CURRENT)

mattmight.bib:
	$(WGETMATTMIGHTBIB)

matt-might-macros.tex: 
	wget http://matt.might.net/share/tex/matt-might-macros.tex

%.dvi: %.tex abstract.tex content.tex local-macros.tex matt-might-macros.tex
	latex $(basename $@)

%.pdf: *.tex
	pdflatex $(basename $@)
#	dvipdfm -o $(basename $@).pdf $(basename $@).dvi

flush: clean
	rm -f matt-might-macros.tex mattmight.bib

submit: $(CURRENT).pdf 
	mv $(CURRENT).pdf icfp058fp-might.pdf
	pdf2ps icfp058fp-might.pdf
	cat abstract.tex content.tex > icfp058fp-might.tex

open:
	open paper-hosc.pdf

clean:
	rm -f {paper-lncs,paper-sigproc,paper-sigplan}.{dvi,ps,pdf,log,toc,blg,bbl,aux,rel} *.log icfp085fp-might.*


