#! /usr/bin/env python

###put files into folders

import os
import glob

with open("squamata_names.txt", 'r') as a:
	x = a.read().splitlines()

with open("testudines_names.txt", 'r') as b:
	y = b.read().splitlines()
	
with open("crocodylia_names.txt", 'r') as c:
	z = c.read().splitlines()
	
cwd = os.getcwd()
dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	name = os.path.basename(d)
#	print name
	if name in x:
		print name
		os.rename(d, cwd+'/Squamata/'+name)

for d in dirs:
	name = os.path.basename(d)
#	print name
	if name in y:
		print name
		os.rename(d, cwd+'/Testudines/'+name)

for d in dirs:
	name = os.path.basename(d)
#	print name
	if name in z:
		print name
		os.rename(d, cwd+'/Crocodylia/'+name)