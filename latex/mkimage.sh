#!/bin/bash

pdflatex $1".tex"
convert -quality 00 -density 300x300 -flatten $1".pdf" $1".png"
