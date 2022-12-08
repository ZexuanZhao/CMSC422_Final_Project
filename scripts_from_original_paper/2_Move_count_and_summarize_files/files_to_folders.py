#! /usr/bin/env python

###put files into folders

import os
import glob

cwd = os.getcwd()
for f in glob.glob('*.txt'):
	dir_name = os.path.splitext(f)[0]
	print dir_name
	os.mkdir(os.path.join(cwd, dir_name))
	os.rename(f, dir_name+'/'+f)