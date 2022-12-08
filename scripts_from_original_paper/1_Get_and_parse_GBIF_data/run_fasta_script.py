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
	print cwd
	cmd = ("python /Users/Tara/Dropbox/2016project/SCRIPTS/fasta_files.py")
	os.system(cmd)
	