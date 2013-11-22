#!/bin/bash
name="thesis-paper"
if [$1 != ""]; then
	$name = $1
fi
rm $name.aux
rm $name.bbl
rm $name.blg
rm $name.log
rm $name.pdf
rm $name.lol
rm $name.lot
rm $name.lof
rm $name.out
rm $name.toc
pdf-slatex $name.tex
bibtex $name
pdflatex $name.tex
pdflatex $name.tex
pdflatex $name.tex
