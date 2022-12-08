MMRR<-function(Y,X,nperm=999){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,unfold)
  fit<-lm(y~Xmats)
  coeffs<-fit$coefficients
  summ<-summary(fit)
  r.squared<-summ$r.squared
  tstat<-summ$coefficients[,"t value"]
  Fstat<-summ$fstatistic[1]
  tprob<-rep(1,length(tstat))
  Fprob<-1
  
  #perform permutations
  for(i in 1:nperm){
    rand<-sample(1:nrowsY)
    Yperm<-Y[rand,rand]
    yperm<-unfold(Yperm)
    fit<-lm(yperm~Xmats)
    summ<-summary(fit)
    Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
    tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
  }
  
  #return values
  tp<-tprob/(nperm+1)
  Fp<-Fprob/(nperm+1)
  names(r.squared)<-"r.squared"
  names(coeffs)<-c("Intercept",names(X))
  names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat)<-"F-statistic"
  names(Fp)<-"F p-value"
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp))
}

# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR

unfold<-function(X){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=TRUE, scale=TRUE)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}

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

#get distance matrix for DNA data
mydata.dnadist <- dist.gene(mydata.dna)
#mydata.dnadist

#get gps coordinates that match the ones from the fasta file we are analyzing
gps <- read.table(args[3], sep="\t")
gps <- gps[order(gps[,2]),]
#remove duplicate genbank accessions
gps <- gps[!duplicated(gps[,2]),]
#gps
mydata.coord <- gps[gps[,2] %in% IDnames,]
#mydata.coord

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

#import bioclim data
# w for worldwide
bio1w = raster("~/Desktop/BIOCLIM/bio_1.bil")
bio2w = raster("~/Desktop/BIOCLIM/bio_2.bil")
bio3w = raster("~/Desktop/BIOCLIM/bio_3.bil")
bio4w = raster("~/Desktop/BIOCLIM/bio_4.bil")
bio5w = raster("~/Desktop/BIOCLIM/bio_5.bil")
bio6w = raster("~/Desktop/BIOCLIM/bio_6.bil")
bio7w = raster("~/Desktop/BIOCLIM/bio_7.bil")
bio8w = raster("~/Desktop/BIOCLIM/bio_8.bil")
bio9w = raster("~/Desktop/BIOCLIM/bio_9.bil")
bio10w = raster("~/Desktop/BIOCLIM/bio_10.bil")
bio11w = raster("~/Desktop/BIOCLIM/bio_11.bil")
bio12w = raster("~/Desktop/BIOCLIM/bio_12.bil")
bio13w = raster("~/Desktop/BIOCLIM/bio_13.bil")
bio14w = raster("~/Desktop/BIOCLIM/bio_14.bil")
bio15w = raster("~/Desktop/BIOCLIM/bio_15.bil")
bio16w = raster("~/Desktop/BIOCLIM/bio_16.bil")
bio17w = raster("~/Desktop/BIOCLIM/bio_17.bil")
bio18w = raster("~/Desktop/BIOCLIM/bio_18.bil")
bio19w = raster("~/Desktop/BIOCLIM/bio_19.bil")

bios = stack(bio1w, bio2w, bio3w, bio4w, bio5w, bio6w, bio7w, bio8w, bio9w, bio10w, bio11w, bio12w, bio13w, bio14w, bio15w, bio16w, bio17w, bio18w, bio19w)

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

mydata.envdist <- dist(pc1, method="euclidean", diag=TRUE, upper=TRUE)

#length(mydata.envdist)
#length(mydata.geodist)
#length(mydata.dnadist)

dna<-as.matrix(mydata.dnadist)
geo<-as.matrix(mydata.geodist)
env<-as.matrix(mydata.envdist)

Xmats <- list(geography=geo,environment=env)
m<-MMRR(dna,Xmats,nperm=999)

r<-m$r.squared
c<-m$coefficients[1]
cgeo<-m$coefficients[2]
cenv<-m$coefficients[3]
t<-m$tstatistic[1]
tgeo<-m$tstatistic[2]
tenv<-m$tstatistic[3]
p<-m$tpvalue[1]
pgeo<-m$tpvalue[2]
penv<-m$tpvalue[3]
f<-m$Fstatistic
fp<-m$Fpvalue

setwd(args[4])
write.table(data.frame(species, gene, n, r,c,t,p,f,fp, cgeo,tgeo,pgeo, cenv,tenv,penv), file = "MMRR.txt", sep = "\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)

rm(list = ls())

###############################################
#vp<- adonis(dna, geo, env, method="euclidean")
#sem<-sem(dna, geo, env)