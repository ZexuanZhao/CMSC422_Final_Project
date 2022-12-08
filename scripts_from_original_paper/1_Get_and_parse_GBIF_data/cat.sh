#!/bin/bash

#cats files that are the same gene
shopt -s nullglob
while IFS= read -r pattern; do
    files=( *_"$pattern"_*.fa )
    if [[ "${#files[@]}" -lt 3 ]]; then
        echo "not enough files match pattern *$pattern*.fa"
    else
        cat "${files[@]}" > $pattern.fasta
    fi
done < $1 #/Users/path/genenames.txt

#MAKE if .afa file DO ALIGNMENT
#muscle alignment
#/Users/Tara/Desktop/Tara2016/SCRIPTS/muscle -in COII.fasta -out COII.afa