setwd("")

library(raster)
library(maps)

#SPECIES COUNTS MAP
#import all species occurence layers all at once
species_raster <- lapply(Sys.glob("*_polygon.tif"), raster)

#par(mfrow=c(1,2))
#plot(species_raster[[1]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
#plot(species_raster[[2]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)

#extent of North and South America
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

#get all rasters to same extent
for(i in 1:length(species_raster)) {
    species_raster[[i]] <- resample(species_raster[[i]], r1)
}
#change all NAs to 0
for(i in 1:length(species_raster)) {
    species_raster[[i]] [is.na(species_raster[[i]])]<-0
}

#dev.new()
#par(mfrow=c(1,2))
#plot(species_raster[[1]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
#plot(species_raster[[2]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)

species_stack<-stack(species_raster)
species_layers<-overlay(species_stack, fun=sum)

#dev.new()
#plot(species_layers)

jpeg(filename="species_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(species_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(species_layers, file="total_species.tif", overwrite=T)

#SPECIES NUCLEOTIDE MAP
#import all species occurence layers all at once
nucdiv_raster <- lapply(Sys.glob("*_nucdiv.tif"), raster)

#get all rasters to same extent
for(i in 1:length(nucdiv_raster)) {
    nucdiv_raster[[i]] <- resample(nucdiv_raster[[i]], r1)
}
#change all NAs to 0
for(i in 1:length(nucdiv_raster)) {
    nucdiv_raster[[i]] [is.na(nucdiv_raster[[i]])]<-0
}

nucdiv_stack<-stack(nucdiv_raster)
nucdiv_layers<-overlay(nucdiv_stack, fun=sum)

#dev.new()
#plot(nucdiv_layers)

jpeg(filename="nucdiv_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(nucdiv_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(nucdiv_layers, file="total_nucdiv.tif", overwrite=T)