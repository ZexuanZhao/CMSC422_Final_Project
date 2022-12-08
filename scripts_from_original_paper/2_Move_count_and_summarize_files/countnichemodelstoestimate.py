#! /usr/bin/env python

import os
import sys

with open ('est.txt', 'w') as e:

	wd = os.getcwd()

	folders = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
	for f in folders:
		os.chdir(f)
		dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
		count = 0
		for d in dirs:
			os.chdir(d)
			species = (os.path.splitext(os.path.basename(d))[0])
			gpscheck = sum(1 for line in open(species+".txt"))
			if gpscheck > 1:
				count += 1
	
		folder = (os.path.splitext(os.path.basename(f))[0])
		print ("There are "+str(int(count))+" "+folder+" niche models to estimate")
		e.write("There are "+str(int(count))+" "+folder+" niche models to estimate"+"\n")		