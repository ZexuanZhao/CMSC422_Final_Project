#! /usr/bin/env python

import os
import glob

root = os.getcwd()

with open("ibe_names.txt", 'r') as s:
	list = s.read().splitlines()
	
dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	
	dirs2 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#	print dirs2
	for d2 in dirs2:
		os.chdir(d2)
		species = os.path.basename(d2)
		if species in list:
			cmd = 'Rscript /Users/tara/Dropbox/2016project/SCRIPTS/6_Diversity_heat_maps/species_points_map.r "'+d2+'" "'+species+'.txt"'
			print cmd
			os.system(cmd)
