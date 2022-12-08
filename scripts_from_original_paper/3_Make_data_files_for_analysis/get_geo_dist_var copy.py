#! /usr/bin/env python

import os
import re

root = os.getcwd()


dir = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
for e in dir:
	species = os.path.basename(e)
	print e
	print species

	cmd1 = ('Rscript /Users/Tara/Dropbox/IBDE/SCRIPTS/get_geo_dist_var.r "'+e+'" "'+species+'.txt"')
	print cmd1
	os.system(cmd1)

	
os.chdir(root)
os.system('cat ./**/geo_dist_var.txt >geo_dist_var.txt')
