#! /usr/bin/env python

from Bio import SeqIO
import os
import sys
import itertools
import glob
import re

###gets .fa files for each sequence. they are labeled as accession#_gene_species.fa
filename = "sequence.gb"

filebase = os.path.splitext(filename)[0]

handle = open(filename, 'rU')

all_genes = []

for record in SeqIO.parse(handle, 'genbank') :
	
	seq = str(record.seq)
	
	for feature in record.features:
		#print feature.type
		#print feature.location
	
		if feature.type == 'CDS': #or feature.type == 'rRNA':
				
			if 'gene' in feature.qualifiers:
				geneName = feature.qualifiers['gene'][0]
			elif 'product' in feature.qualifiers:
				geneName = feature.qualifiers['product'][0]
			else:
				print 'ERROR when parsing feature:'
				print feature.qualifiers
				
			
			geneName = geneName.replace(' ', '-')
			geneName = geneName.replace('/', '-')
			geneName = geneName.replace("'", '') 
			geneName = geneName.replace(".", '') #THIS
			all_genes.append(geneName)
			#print geneName
			
			speciesName = record.annotations["organism"]
			speciesName = speciesName.replace('/', ' ')
			
			geneFile = open(record.name + '_' + geneName + '_' + speciesName +'.fa', 'w')
			geneFile.write('>')
			geneFile.write(os.path.basename(record.name) + '_' + geneName + "\n")
			geneFile.write(seq[feature.location.start.position:feature.location.end.position])
			geneFile.write("\n")
		

print "------ fasta files retrieved"


###uses the third part of the file name (id_gene_species.fa) to make directories and put files there
#subroutine that gets directory name
def get_dir_name(filename):
    pos1 = filename.rfind('_')
    pos2 = filename.find('.')
    return filename[pos1+1:pos2]

#makes folder and puts .fa files in right place
for f in glob.glob('*.fa'):
    cwd = os.getcwd()
    dir_name = os.path.join(cwd+'/'+get_dir_name(f))
#    print dir_name
    if not os.path.exists(dir_name):
        os.mkdir(dir_name)
    os.rename(f, dir_name+'/'+f)
  
print "------ fasta files put into folers"

#get directory names
dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]

###get list of species folder and make text file with list of names
s = open('speciesnames.txt', 'w')
for d in dirs:
	species = (os.path.splitext(os.path.basename(d))[0])
	s.write(species+'\n')
s.close()

print "------ speciesnames.txt file written"
print ('------ THERE ARE ' + str(int(len(dirs))) + ' SPECIES.')
 

###get gps points for each species into text file
filename = "GPS.txt"
x = open(filename, 'r')

for d in dirs:
    names = (os.path.splitext(os.path.basename(d))[0])
    x.seek(0)
    with open(str(names) + ".txt", "w") as y:
        for line in x:
            if line.startswith(names):
                y.write(line)
x.close()
y.close()

#push gps species files into species folder
files = filter(os.path.isfile, os.listdir('.'))

for t in files:
	base = (os.path.splitext(os.path.basename(t))[0])
	for d in dirs:
		name = os.path.basename(d)
		if name == base:
			os.rename(t, d+'/'+t)
			
print "------ gps files written and put into folders"


###makes txt file with list of genenames		
Unique_genes = set(all_genes)
Genes = list(Unique_genes)		
g = open('genenames.txt', 'w')
for names in Genes:
	g.write(names+'\n')
g.close()

print "------ genenames.txt file written"			