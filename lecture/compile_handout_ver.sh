#!/usr/bin/env bash

filename=$1
sed -i '' "s/\\documentclass\[18pt\]/\\documentclass\[18pt, handout\]/g" $filename
pdflatex $filename
mv "${filename%.*}.pdf" "${filename%.*}_handout.pdf"
sed -i '' "s/\\documentclass\[18pt, handout\]/\\documentclass\[18pt\]/g" $filename