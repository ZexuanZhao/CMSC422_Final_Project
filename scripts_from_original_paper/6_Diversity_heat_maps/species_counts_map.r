library(geosphere)
library(raster)
library(plyr)

args = commandArgs(trailingOnly=TRUE)

setwd(args[1])
wd<-getwd()

species<-basename(wd)

gps<-read.table(args[2])
xy<-gps[,c(5,4)]

#draw convex hull polygon
ch <- chull(xy)
coords <- xy[c(ch, ch[1]), ]
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))

#plot(xy, pch=19)
#lines(coords, col="red")

r <- raster(nrows=5000, ncols=5000)
extent(r)<-extent(sp_poly)
ras<-rasterize(sp_poly, r, filename=(paste(species, "_polygon.tiff", sep="")))
#plot(ras)

#FINAL STEP USE species_diversity_heatmap.r




		
			