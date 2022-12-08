#! /usr/bin/env python

import os
import re

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	cmd1 = ('Rscript /Users/Tara/Dropbox/2016project/SCRIPTS/get_taxonomy_lists.r "'+d+'" DATA.txt')
	print cmd1
	os.system(cmd1)

os.chdir(root)
group = os.path.basename(root)

os.system('cat ./**/family.txt >' +group+'_family.txt')
os.system('cat ./**/genus.txt >' +group+'_genus.txt')

num_lines_f = sum(1 for f in open(group+'_family.txt'))
num_lines_g = sum(1 for g in open(group+'_genus.txt'))

os.rename(group+'_family.txt', group+'_family_'+str(int(num_lines_f))+'.txt')
os.rename(group+'_genus.txt', group+'_genus_'+str(int(num_lines_g))+'.txt')