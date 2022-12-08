#! /usr/bin/env python

import os
import sys

#get in correct directory

with open('tax_column_headings.txt', 'w') as t:
	t.write("species"+"\t"+"gene"+"\t"+"n"+"\t"+"p"+"\t"+"genus"+"\t"+"family"+"\t"+"order"+"\t"+"class"+"\t"+"phylum"+"\t"+"kingdom"+"\t"+"metobolism"+"\t"+"environment")

with open('biome_column_headings.txt', 'w') as b:
	b.write("11"+"\t"+"14"+"\t"+"20"+"\t"+"30"+"\t"+"40"+"\t"+"50"+"\t"+"60"+"\t"+"70"+"\t"+"90"+"\t"+"100"+"\t"+"110"+"\t"+"120"+"\t"+"130"+"\t"+"140"+"\t"+"150"+"\t"+"160"+"\t"+"170"+"\t"+"180"+"\t"+"190"+"\t"+"200"+"\t"+"210"+"\t"+"220"+"\t"+"230"+"\t"+"area"+"\t"+"max_lat"+"\t"+"min_lat"+"\t"+"max_lon"+"\t"+"min_lon")
	
os.system('paste tax_column_headings.txt biome_column_headings.txt >column_headings.txt')
os.system('rm tax_column_headings.txt biome_column_headings.txt')
