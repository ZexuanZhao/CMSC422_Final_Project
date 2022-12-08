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
	cmd = ("python /Users/Tara/Dropbox/2016project/SCRIPTS/align.py")
	os.system(cmd)

os.chdir(root)
a = open('alignments.txt', 'w')

for d in dirs:
	os.chdir(d)
	cwd = os.getcwd()
	print cwd

	lst = []
	l = open('list.txt', 'r')
	for line in l:
    		lst.append(line.split('\t')[0])
    	print (str(int(len(set(lst)))) + ' ' + "SPECIES WITH .afa FILES")
	a.write(cwd+'\n'+str(int(len(set(lst))))+ " SPECIES" + '\n')


l.close()
a.close()