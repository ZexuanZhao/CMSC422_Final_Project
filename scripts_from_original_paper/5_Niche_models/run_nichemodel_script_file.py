#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()
print root

filenames = glob.glob("*.txt")
for f in filenames:
	print f
	species = (os.path.splitext(os.path.basename(f))[0])
	print species
	cmd = 'Rscript /Users/tara/Dropbox/2016project/SCRIPTS/5_Niche_models/nichemodel.r "'+root+'" "'+f+'" "'+species+'"'
	print cmd
	os.system(cmd)
