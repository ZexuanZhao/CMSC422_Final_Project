#!/usr/bin/env bash

for d in */ ; do

	test -d "$d" || continue
	echo $d
	cd $d 

	cut -f 4 taxonomy.txt | uniq | wc -l
	
	cd ..
done