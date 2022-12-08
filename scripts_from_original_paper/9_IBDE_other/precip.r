library(raster)

args = commandArgs(trailingOnly=TRUE)
setwd(args[1])
wd<-getwd()
species <- basename(wd)
data<-read.table(args[2])
points<-data[,c(5,4)]

bio12 = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_12.bil")
bio15 = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_15.bil")

precip<-extract(bio12, points)
mean_precip<-mean(precip, na.rm=TRUE)

season<-extract(bio15, points)
mean_season<-mean(season, na.rm=TRUE)

setwd(args[3])
write.table(data.frame(species, mean_precip, mean_season), file="precip.txt", sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)

