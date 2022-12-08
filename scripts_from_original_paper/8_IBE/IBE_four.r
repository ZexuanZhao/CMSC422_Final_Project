library(raster)
library(ape)
library(tools)
library(ecodist)
library(vegan)

args = commandArgs(trailingOnly=TRUE)
setwd(args[1])
wd<-getwd()
species <- basename(wd)

###GEOGRAPHIC AND GENETIC DIST MATRICES

#get IDs (accession numbers) from .afa file
mydata.dna <- read.dna(args[2], format="fasta")
#mydata.dna
IDnames <- labels(mydata.dna)
IDnames <- substr(IDnames, 1,8)
IDnames

gene <- file_path_sans_ext(args[2])

#get gps coordinates that match the ones from the fasta file we are analyzing
gps <- read.table(args[3], sep="\t")
gps <- gps[order(gps[,2]),]
#remove duplicate genbank accessions
gps <- gps[!duplicated(gps[,2]),]
#gps
mydata.coord <- gps[gps[,2] %in% IDnames,]
#mydata.coord

#get distance matrix for DNA data
mydata.dnadist <- dist.gene(mydata.dna)
#mydata.dnadist

# Geographical distance
mydata.geodist <- dist(mydata.coord[,4], mydata.coord[,3], method="euclidean", diag=TRUE, upper=TRUE)
#mydata.geodist

#REMOVE ENTRIES FROM DNA THAT ARE NOT IN GPS SO MATRICES ARE SAME SIZE
l1 <- length(mydata.dnadist)
l2 <- length(mydata.geodist)
if (l1 != l2) {
	IDs<-mydata.coord$V2
	IDs
	list <- as.vector(substr(labels(mydata.dnadist), 1,8))
	list
	z<-full(mydata.dnadist)
	rownames(z)<-list
	colnames(z)<-list
	c <- z[IDs,IDs]
	mydata.dnadist<-as.dist(c)
	}

n <- length(labels(mydata.dnadist))

### ENV DIST MATRIX - RUN SCRIPT SO THESE ARE ONLY ADDED ONE TIME

#import bioclim data
# w for worldwide
bio1w = raster("~/Desktop/BIOCLIM/bio_30seconds_four/bio_1.bil")
bio2w = raster("~/Desktop/BIOCLIM/bio_30seconds_four/bio_4.bil")
bio3w = raster("~/Desktop/BIOCLIM/bio_30seconds_four/bio_12.bil")
bio4w = raster("~/Desktop/BIOCLIM/bio_30seconds_four/bio_15.bil")

bios = stack(bio1w, bio2w, bio3w, bio4w)

#raster value from gps points
g<-mydata.coord[,c(4,3)]
env_var <- extract(bios, g)
#env_var

#imputes median from bios columns if NA is present (prcomp won't run with NAs)
f=function(x){
   x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
   x #display the column
}
env_var=data.frame(apply(env_var,2,f))

#do PCA on env variables to get one number per gps point
pca<-prcomp(env_var)
#get PC1 values for each individual
scores<-as.data.frame(pca$x)
pc1<-scores[,1]

#get PC %
vars <- apply(pca$x, 2, var)  
props <- vars / sum(vars)
prop <- cumsum(props[1])

mydata.envdist <- dist(pc1, method="euclidean", diag=TRUE, upper=TRUE)
#length(mydata.envdist)
#length(mydata.geodist)
#length(mydata.dnadist)

###PARTIAL MANTEL TESTS

pmt <- mantel.partial(mydata.dnadist, mydata.geodist, mydata.envdist, method = "pearson", permutations = 999)
#pmt
pmtp <- pmt$signif
pmtr <- pmt$statistic
#pmtp
#pmtr

dg.mantel <- mantel(mydata.dnadist, mydata.geodist)
de.mantel <- mantel(mydata.dnadist, mydata.envdist)
ge.mantel <- mantel(mydata.geodist, mydata.envdist)
#dg.mantel
#de.mantel
#ge.mantel

dgp<-dg.mantel$signif
dgr<-dg.mantel$statistic
dep<-de.mantel$signif
der<-de.mantel$statistic
gep<-ge.mantel$signif
ger<-ge.mantel$statistic
dgp
dgr
dep
der
gep
ger

###GET R AND P FROM ALL ANALYSES FOR LATER ANALYSES
setwd(args[4])
write.table(data.frame(species, gene, n, pmtr, pmtp, dgr, dgp, der, dep, ger, gep, prop), file = "ibe_four.txt", sep = "\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)

rm(list = ls())


