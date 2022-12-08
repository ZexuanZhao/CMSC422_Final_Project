#!/usr/bin/env bash

#remove lines without species name
awk -F'\t' '$1!=""' DATA.txt >DATA_temp.txt

#remove duplicate species lines
awk -F'\t' '!seen[$1]++' DATA_temp.txt >DATA_unique.txt
rm DATA_temp.txt

#get only taxonomy info
cut -f -7 DATA_unique.txt >taxonomy.txt