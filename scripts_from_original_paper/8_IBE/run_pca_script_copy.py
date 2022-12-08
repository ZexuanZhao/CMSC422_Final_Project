#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()

dir2 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
for d2 in dir2:
	os.chdir(d2)
	cwd = os.getcwd()
	species = (os.path.splitext(os.path.basename(d2))[0])
#	print cwd
#	print species
	files_to_analyze = (glob.glob("*.afa"))
	for f in files_to_analyze:
#		print f
		cmd = 'Rscript /Users/tara/Dropbox/2016project/SCRIPTS/8_IBE/get_PCA_data.r "'+cwd+'" "'+species+'.txt"'
		print cmd
		os.system(cmd)
