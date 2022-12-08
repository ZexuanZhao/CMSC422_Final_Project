#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()

dirsy = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

counter = 0

for y in dirsy:
	os.chdir(y)
	dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
	print y
	
	
	for d in dirs:
		os.chdir(d)
#		cwd = os.getcwd()
#		print cwd
		fa_files_to_count = (glob.glob("*.fa"))
		fa = len(fa_files_to_count)
#		print fa
		counter += fa
		
print counter