#! /usr/bin/env python

import os
import glob
import re

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	cmd1 = ("python /Users/Tara/Dropbox/2016project/SCRIPTS/ibd_call_R.py")
	os.system(cmd1)

for d in dirs:
	try:
		os.chdir(d)
		print d
		os.system('paste -d "\t\n" list.txt p.txt >results_temp.txt')
		invalid = ('invalid')
		with open('results_temp.txt') as oldfile, open('RESULTS.txt', 'w') as newfile:
    			for line in oldfile:
        			if not (invalid in line):
            				newfile.write(line) 
		num_lines = sum(1 for line in open('RESULTS.txt'))
		print ('------ '+str(int(num_lines))+' IBD analyses were run.')
		os.system("rm results_temp.txt")
		#os.system("rm list.txt p.txt")
	except IOError:
		print "------ There are no files to analyze"

os.chdir(root)
cmd2 = ("cat ./**/RESULTS.txt >RESULTS.txt")
os.system(cmd2)