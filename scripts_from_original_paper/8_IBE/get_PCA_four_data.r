library(raster)
library(tools)
library(ecodist)

args = commandArgs(trailingOnly=TRUE)
setwd(args[1])
wd<-getwd()
species <- basename(wd)

#get gps coordinates
gps <- read.table(args[2], sep="\t")

#remove duplicates
gps <- gps[,c(4,3)]
gps <- unique(gps)

#import bioclim data
# w for worldwide
bio1w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_1.bil")
bio4w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_4.bil")
bio12w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_12.bil")
bio15w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_15.bil")

bios = stack(bio1w, bio4w, bio12w, bio15w)

#raster value from gps points
env_var <- extract(bios, gps)
#env_var

#imputes median from bios columns if NA is present (prcomp won't run with NAs)
f=function(x){
   x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
   x #display the column
}
env_var=data.frame(apply(env_var,2,f))

#do PCA on env variables to get one number per gps point
pca<-prcomp(env_var)

#get PC %
vars <- apply(pca$x, 2, var)  
props <- vars / sum(vars)
prop <- cumsum(props[1])


###GET % FROM ALL ANALYSES FOR LATER ANALYSES
setwd("/Volumes/HD2/Tara2016/DATA")
write.table(data.frame(species, prop), sep="\t", file = "pca.txt", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)

rm(list = ls())


