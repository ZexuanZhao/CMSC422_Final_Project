import glob, os

##########################	
#code to move ascii files#
##########################

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

for d in dirs:
	os.chdir(d)
	
	dirs2 = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
	for d2 in dirs2:
		species = os.path.basename(d2)
		print species

		for a in glob.glob('*.tif'):
			os.rename(a, root+'/asc_files/'+a)
				

