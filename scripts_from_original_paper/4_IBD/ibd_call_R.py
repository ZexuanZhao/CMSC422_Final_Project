#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	cwd = os.getcwd()
	species = (os.path.splitext(os.path.basename(d))[0])
	print cwd
	print species
	files_to_analyze = (glob.glob("*.afa"))
	for f in files_to_analyze:
		print f
		cmd = 'Rscript /Users/Tara/Dropbox/2016project/SCRIPTS/ibd.r '+f+' "'+species+'.txt" "'+root+'/p.txt"'
		print cmd
		os.system(cmd)
