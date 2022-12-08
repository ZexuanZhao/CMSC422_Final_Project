#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()

dir1 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dir1

for d1 in dir1:
	os.chdir(d1)
	print "YOU ARE IN DATA FOLDER"+d1
	dir2 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
	for d2 in dir2:
		os.chdir(d2)
		cwd = os.getcwd()
		species = (os.path.splitext(os.path.basename(d2))[0])
#		print cwd
#		print species
		files_to_analyze = (glob.glob("*.afa"))
		for f in files_to_analyze:
#			print f
			cmd = 'Rscript /Users/tara/Dropbox/IBDE/SCRIPTS/6_Diversity_heat_maps/pi.r "'+cwd+'" '+f+' '+root+''
			print cmd
			os.system(cmd)
