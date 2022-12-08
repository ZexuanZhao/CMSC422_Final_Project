import glob, os

#############################	
#rm niche folder with gps <3#
#############################

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]

for d in dirs:
	os.chdir(d)
	species = os.path.basename(d)
	print species
	file = species + ".txt"
	os.system('cut -f 3,4 "'+file+'" >temp.txt')
	
	#get number of gps points
	with open ("temp.txt", "r") as t:
		uniqlines = set(t.readlines())
	number =  len(uniqlines)
	
	print number
	
	if number <3:
		os.system("rm -r NicheModel")
				
os.chdir(root)
os.system("rm ./**/temp.txt")