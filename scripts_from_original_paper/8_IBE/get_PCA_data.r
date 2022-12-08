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
bio2w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_2.bil")
bio3w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_3.bil")
bio4w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_4.bil")
bio5w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_5.bil")
bio6w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_6.bil")
bio7w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_7.bil")
bio8w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_8.bil")
bio9w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_9.bil")
bio10w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_10.bil")
bio11w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_11.bil")
bio12w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_12.bil")
bio13w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_13.bil")
bio14w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_14.bil")
bio15w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_15.bil")
bio16w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_16.bil")
bio17w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_17.bil")
bio18w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_18.bil")
bio19w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_19.bil")

bios = stack(bio1w, bio2w, bio3w, bio4w, bio5w, bio6w, bio7w, bio8w, bio9w, bio10w, bio11w, bio12w, bio13w, bio14w, bio15w, bio16w, bio17w, bio18w, bio19w)

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


