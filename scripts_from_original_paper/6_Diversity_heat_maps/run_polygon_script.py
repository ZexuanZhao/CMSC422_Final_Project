#! /usr/bin/env python

import os
import glob


root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

with open("species_gps.txt", 'r') as s:
	list = s.read().splitlines()

for d in dirs:
	os.chdir(d)
	species = os.path.basename(d)
	if species in list:
		cmd = 'Rscript /Users/Tara/Dropbox/2016project/SCRIPTS/polygon_species.r "'+d+'" "'+species+'.txt"'
		print cmd
		os.system(cmd)
