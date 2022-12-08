library(geosphere)
library(raster)
library(plyr)

args = commandArgs(trailingOnly=TRUE)

setwd(args[1])
wd<-getwd()

species<-basename(wd)

gps<-read.table(args[2])

#to get max and min lat and lon for each species
max_lat <- max(gps$V4)
min_lat <- min(gps$V4)
max_lon <- max(gps$V5)
min_lon <- min(gps$V5)

xy<-gps[,c(5,4)]

#draw convex hull polygon
ch <- chull(xy)
coords <- xy[c(ch, ch[1]), ]

#plot(xy, pch=19)
#lines(coords, col="red")

#get area for polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
area<-areaPolygon(sp_poly)
format(area, scientific = FALSE)

dist<-data.frame(area, max_lat, min_lat, max_lon, min_lon)

#import global cover data to determine biomes per species
cover<-raster("~/Desktop/Tara2016/Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")

#raster values by points
pt<-extract(cover, xy)
#raster value by polygon
#pg<-extract(cover, sp_poly)

a<-as.data.frame(table(pt))
sum<-sum(a$Freq)

#proportion of points in each biome
temp <- ddply(a,.(pt),transform,prop=Freq/sum(a$Freq))
prop<-data.frame(temp[,c(1,3)])

#biome labels
biome<-read.table("~/Desktop/Tara2016/Globcover2009_V2.3_Global_/biome.txt", header=TRUE)

results<-merge(biome, prop, all.x = TRUE, by.x = "Biome", by.y = "pt", col.names=FALSE)
results[is.na(results)] <-0
results<-t(results)
results<-as.list(results[2,])

write.table(data.frame(species, results, dist), file = "geo_dist_var.txt", sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)

		
			