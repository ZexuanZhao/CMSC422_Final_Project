#! /usr/bin/env python

import os
import sys
import glob

cwd = os.getcwd()
#print('cwd = ' + cwd)
genenamespath = os.path.join(cwd + '/' + 'genenames.txt')

#cat files and make alignments
for root, subdirs, files in os.walk(cwd):
	for subdir in subdirs:
		newwd = os.path.join(root + '/' + subdir)
		os.chdir(newwd)
		print ("WE ARE IN FOLDER:")
		print newwd
		#calling bash script that cats .fa files now called .fasta files
		cmd = "sh /Users/tarapelletier/Dropbox/IBDE/SCRIPTS/1_Get_and_parse_GBIF_data/cat.sh "+genenamespath+""
		print cmd
		os.system(cmd)
		length = len(glob.glob("*.fasta"))
		print length
		files_to_align = (glob.glob("*.fasta"))
		for t in files_to_align:
			name = (os.path.splitext(os.path.basename(t))[0])
			#print name
			#calling muscle to align files now called .afa files
			cmd1 = "/Users/tarapelletier/Dropbox/IBDE/SCRIPTS/1_Get_and_parse_GBIF_data/muscle -in "+name+".fasta -out "+name+"_temp.afa"
			cmd2 = "python /Users/tarapelletier/Dropbox/IBDE/SCRIPTS/1_Get_and_parse_GBIF_data/stable.py "+name+".fasta "+name+"_temp.afa > "+name+".afa"
			os.system(cmd1)
			os.system(cmd2)
			os.system("rm "+name+"_temp.afa")