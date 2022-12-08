#! /usr/bin/env python

import os
import re

root = os.getcwd()

dir = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
for e in dir:
	species = os.path.basename(e)
	print e
#	print species

	cmd1 = ('Rscript /Users/tara/Dropbox/IBDE/SCRIPTS/3_Make_data_files_for_analysis/get_additional_geo_dist_var.r "'+e+'" "'+species+'.txt" '+root+'')
#	print cmd1
	os.system(cmd1)

