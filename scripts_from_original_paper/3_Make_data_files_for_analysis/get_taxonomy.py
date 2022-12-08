#! /usr/bin/env python

import os
import sys
import re

main = os.getcwd()
dirs1 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]

#to pull out taxon info for each species
for d in dirs1:
	os.chdir(d)
#	cwd=os.getcwd()
#	names = (os.path.splitext(os.path.basename(cwd))[0])
#	print ("getting data in folder: "+names)
	cmd1 = ("bash /Users/Tara/Dropbox/2016project/SCRIPTS/get_taxonomy.sh")
#	print cmd1
	os.system(cmd1)

#cat all taxonomy files from each folder
os.chdir(main)
os.system("cat ./**/taxonomy.txt >taxonomy.txt")

#add additional variables
cmd2 = ('Rscript /Users/Tara/Dropbox/2016project/SCRIPTS/get_other_var.r "'+main+'" taxonomy.txt')
os.system(cmd2)