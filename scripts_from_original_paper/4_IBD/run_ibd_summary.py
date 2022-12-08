#! /usr/bin/env python

import os
import re

root = os.getcwd()
file = os.path.join(root + '/' + "ibd_summary_temp.txt")

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	results = os.path.join(d + '/' + "RESULTS.txt")
	#print results
	group = os.path.basename(d)
	#print group

	cmd1 = ('Rscript /Users/Tara/Dropbox/2016project/SCRIPTS/4_IBD/ibd_summary.r '+results+' '+group+' '+file+'')
	os.system(cmd1)

os.chdir(root)
a = open ('columnheadings.txt', 'w')
a.write("Group"+'\t'+"n"+'\t'+"x"+'\t'+"prop"+'\t'+"significance"+'\t'+"R-squared"+'\t'+"p"+'\n')
a.close()

z = open ('notes.txt', 'w')
z.write('\n'+"Group = GBIF download"+'\n'+"n = number of IBD analyses"+'\n'+"x = number of IBD analyses significant"+'\n'+"prop = proportion significant"+'\n'+"significance = p-value from test: are more ibd analyses significant than expected by chance (exact binomial test)"+'\n'+"R-squared from linear regression of ibd p-values on ibd n"+'\n'+"p = p-value from linear regression"+'\n')
z.close()

os.system('cat columnheadings.txt ibd_summary_temp.txt notes.txt >ibd_summary.txt')
os.system("rm columnheadings.txt ibd_summary_temp.txt notes.txt")