#! /usr/bin/env python
import os

cmd1 = 'for file in *.fa ; do mv "$file" "${file//cytochrome-b/cytb}" ; done'
cmd2 = 'for file in *.fa ; do mv "$file" "${file//cyt-b/cytb}" ; done'
cmd3 = 'for file in *.fa ; do mv "$file" "${file//Cytb/cytb}" ; done'
cmd4 = 'for file in *.fa ; do mv "$file" "${file//CYTB/cytb}" ; done'
cmd5 = 'for file in *.fa ; do mv "$file" "${file//cytB/cytb}" ; done'
cmd6 = 'for file in *.fa ; do mv "$file" "${file//CO1/COI}" ; done'
cmd7 = 'for file in *.fa ; do mv "$file" "${file//COX1/COI}" ; done'

dirs1 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d1 in dirs1:
	os.chdir(d1)
	os.system("rm ./**/*.fasta")
	os.system("rm ./**/*.afa")
	
	dirs2 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
	#print dirs2

	for d2 in dirs2:
		os.chdir(d2)
		os.system(cmd1)
		os.system(cmd2)
		os.system(cmd3)
		os.system(cmd4)
		os.system(cmd5)
		os.system(cmd6)
		os.system(cmd7)


