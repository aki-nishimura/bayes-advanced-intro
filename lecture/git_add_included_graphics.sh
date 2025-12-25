#!/bin/bash
set -e

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 file.tex"
  exit 1
fi

TEX="$1"

# Common image file extensions (adjust or reorder as needed)
extexts="pdf png jpg jpeg eps"

# Extract all filenames in \includegraphics[<options>]{<filename>}
ggrep -oP '\\includegraphics(?:\[[^\]]*\])?{[^}]+}' "$TEX" |
sed -E 's/\\includegraphics(\[[^]]*\])?{//; s/}$//' |
while IFS= read -r file; do
  # If filename looks like it has an extension
  if [[ "$file" == *.* ]]; then
    if [[ -f "$file" ]]; then
      git add -- "$file"
      echo "Added: $file"
    else
      echo "Warning: file with extension not found: $file" >&2
    fi
  else
    found=0
    for ext in $extexts; do
      img="$file.$ext"
      if [[ -f "$img" ]]; then
        git add -- "$img"
        echo "Added: $img"
        found=1
        break
      fi
    done
    if [[ "$found" -eq 0 ]]; then
      echo "Warning: figure not found for base name: $file" >&2
    fi
  fi
done