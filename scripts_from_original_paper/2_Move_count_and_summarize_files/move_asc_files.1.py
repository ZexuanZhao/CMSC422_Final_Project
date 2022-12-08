import os
import glob

###########################################		
#code to put files back in correct folders#
###########################################

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
print dirs

os.chdir("/Users/Tara/Desktop/Tara2016/DATA/Vertebrates/herps/Amphibia/asc_files")
files = glob.glob("*.asc")
print files

for d in dirs:
	name = os.path.basename(d)
#	print name
		
	for f in files:
		if name == os.path.basename(f).split('_')[0]:
			os.rename(f, d+'/'+f)