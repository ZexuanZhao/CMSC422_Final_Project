#! /usr/bin/env python

import glob, os, shutil

###################	
#code to get files#
###################

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

with open("species_gps.txt", 'r') as s:
	list = s.read().splitlines()
#	print list

for d in dirs:
	os.chdir(d)
	species = os.path.basename(d)
	
	if species in list:
		for a in glob.glob('*.afa'):
			shutil.copy(a, root+'/asc_files/'+species+'_'+a)