#! /usr/bin/env python

import os
import shutil

#path to folder with niche models done
dir_nm = r'/Users/tara/Desktop/mammal_niche_done'
dirpaths_nm = [os.path.abspath(name) for name in os.listdir(dir_nm)]
#print dirpaths_nm


dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
for d in dirs:
	os.chdir(d)
	dirpaths_mm = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#	print dirpaths_mm
	
	for folder_nm in dirpaths_nm:
		name_nm = os.path.basename(folder_nm)
		for folder_mm in dirpaths_mm:
			name_mm = os.path.basename(folder_mm)
			if name_nm in name_mm:
				shutil.copytree(folder_mm, folder_nm)
			

 

