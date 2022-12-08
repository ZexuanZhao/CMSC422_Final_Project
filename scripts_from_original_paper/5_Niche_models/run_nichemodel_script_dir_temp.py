#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	species = (os.path.splitext(os.path.basename(d))[0])
#	num_lines = sum(1 for line in open(species+".txt"))
#	print num_lines
	cmd = 'Rscript /Users/tara/Dropbox/2016project/SCRIPTS/5_Niche_models/nichemodel_temp.r "'+d+'" "'+species+'.txt"'
	print d
	print cmd
	os.system(cmd)
