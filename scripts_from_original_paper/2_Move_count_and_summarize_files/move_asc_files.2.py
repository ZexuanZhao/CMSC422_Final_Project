import glob, os

##########################	
#code to move ascii files#
##########################

root = os.getcwd()

dirs = [os.path.abspath(name) for name in os.listdir(".") if os.path.isdir(name)]
#print dirs

s = open("species_gps.txt", 'w')

#move only those with n>14 and in N & S America
for d in dirs:
	os.chdir(d)
	species = os.path.basename(d)
	print species
	file = species + ".txt"
	os.system('cut -f 3,4 "'+file+'" >temp.txt')
	
	#get longitude
	data=[]
	with open ("temp.txt", "r") as t:
		for line in t:
			fields = line.split('\t')
			data.append(fields)
	try:
		longitude = data[0][1].strip()
	except IndexError:
		pass
	
	#get number of gps points
	with open ("temp.txt", "r") as t:
		uniqlines = set(t.readlines())
	number =  len(uniqlines)
	
	print number
	print longitude
	
	if number > 4 and int(float(longitude)) > -130 and int(float(longitude)) < -60:
			s.write(species)#need to add end of line character
#			print "moving files"
			for a in glob.glob('*.asc'):
				os.rename(a, root+'/asc_files/'+a)
				
s.close()
os.chdir(root)
os.system("rm ./**/temp.txt")
