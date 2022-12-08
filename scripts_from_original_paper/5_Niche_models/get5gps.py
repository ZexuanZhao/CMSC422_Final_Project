#! /usr/bin/env python

#get GPS files that have > 5 unique points

import os
import pandas
import shutil

root = os.getcwd()

dirs1 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs1:
	os.chdir(d)
#	print d
	dirs2 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
	
	for e in dirs2:
		os.chdir(e)
		species = (os.path.splitext(os.path.basename(e))[0])
		gps = (species+".txt")
		print (gps)
		
		file_size = os.path.getsize(gps)
		if file_size > 0:
			df = pandas.read_table (gps, header = None)
			df.columns = ['name', 'id', 'lat', 'lon']
			dfd = df.drop_duplicates(subset=['lat', 'lon'])
			rows = len(dfd.index)
			print (file_size)
#			print (df)
#			print (dfd)
			print (rows)
			
			if rows > 14:
				shutil.copy2(gps, '/Users/tara/Desktop/mammal_niche')