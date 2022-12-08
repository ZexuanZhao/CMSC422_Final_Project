setwd("~/Desktop/Tara2016/DATA/Vertebrates/herps/Amphibia/asc_files")

library(raster)
library(maps)

#CURRENT
#import layers all at once
current_raster <- lapply(Sys.glob("*_current.asc"), raster)

#to see image
#par(mfrow=c(2,2))
#lapply(current_raster, image)

#extent of North and South America
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

#get all rasters to same extent
for(i in 1:length(current_raster)) {
    current_raster[[i]] <- resample(current_raster[[i]], r1)
}
#and make sure all cells are added by changing NAs to 0
for(i in 1:length(current_raster)) {
    current_raster[[i]] [is.na(current_raster[[i]])]<-0
}

#dev.new()
#par(mfrow=c(2,2))
#plot(current_raster[[1]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
#plot(current_raster[[2]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
#plot(current_raster[[3]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
#plot(current_raster[[4]])
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)

current_stack<-stack(current_raster)
current_layers<-overlay(current_stack, fun=sum)

jpeg(filename="current_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(current_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(current_layers, file="current_layers_52.tif", overwrite=T)
rm(current_raster, current_stack)

#mh
mh_raster <- lapply(Sys.glob("*_mh.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(mh_raster)) {
    mh_raster[[i]] <- resample(mh_raster[[i]], r1)
}

for(i in 1:length(mh_raster)) {
    mh_raster[[i]] [is.na(mh_raster[[i]])]<-0
}

mh_stack<-stack(mh_raster)
mh_layers<-overlay(mh_stack, fun=sum)

jpeg(filename="mh_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(mh_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(mh_layers, file="mh_layers_52.tif", overwrite=T)
rm(mh_raster, mh_stack, mh_layers)

#lgm
lgm_raster <- lapply(Sys.glob("*_lgm.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(lgm_raster)) {
    lgm_raster[[i]] <- resample(lgm_raster[[i]], r1)
}

for(i in 1:length(lgm_raster)) {
    lgm_raster[[i]] [is.na(lgm_raster[[i]])]<-0
}

lgm_stack<-stack(lgm_raster)
lgm_layers<-overlay(lgm_stack, fun=sum)

jpeg(filename="lgm_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(lgm_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(lgm_layers, file="lgm_layers_52.tif", overwrite=T)
rm(lgm_raster, lgm_stack, lgm_layers)

#lig
lig_raster <- lapply(Sys.glob("*_lig.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(lig_raster)) {
    lig_raster[[i]] <- resample(lig_raster[[i]], r1)
}

for(i in 1:length(lig_raster)) {
    lig_raster[[i]] [is.na(lig_raster[[i]])]<-0
}

lig_stack<-stack(lig_raster)
lig_layers<-overlay(lig_stack, fun=sum)

jpeg(filename="lig_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(lig_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(lig_layers, file="lig_layers_52.tif", overwrite=T)
rm(lig_raster, lig_stack, lig_layers)

#f2650
f2650_raster <- lapply(Sys.glob("*_f2650.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f2650_raster)) {
    f2650_raster[[i]] <- resample(f2650_raster[[i]], r1)
}

for(i in 1:length(f2650_raster)) {
    f2650_raster[[i]] [is.na(f2650_raster[[i]])]<-0
}

f2650_stack<-stack(f2650_raster)
f2650_layers<-overlay(f2650_stack, fun=sum)

jpeg(filename="f2650_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f2650_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f2650_layers, file="f2650_layers_52.tif", overwrite=T)
rm(f2650_raster, f2650_stack, f2650_layers)

#f2670
f2670_raster <- lapply(Sys.glob("*_f2670.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f2670_raster)) {
    f2670_raster[[i]] <- resample(f2670_raster[[i]], r1)
}

for(i in 1:length(f2670_raster)) {
    f2670_raster[[i]] [is.na(f2670_raster[[i]])]<-0
}

f2670_stack<-stack(f2670_raster)
f2670_layers<-overlay(f2670_stack, fun=sum)

jpeg(filename="f2670_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f2670_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f2670_layers, file="f2670_layers_52.tif", overwrite=T)
rm(f2670_raster, f2670_stack, f2670_layers)

#f4550
f4550_raster <- lapply(Sys.glob("*_f4550.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f4550_raster)) {
    f4550_raster[[i]] <- resample(f4550_raster[[i]], r1)
}

for(i in 1:length(f4550_raster)) {
    f4550_raster[[i]] [is.na(f4550_raster[[i]])]<-0
}

f4550_stack<-stack(f4550_raster)
f4550_layers<-overlay(f4550_stack, fun=sum)

jpeg(filename="f4550_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f4550_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f4550_layers, file="f4550_layers_52.tif", overwrite=T)
rm(f4550_raster, f4550_stack, f4550_layers)

#f4570
f4570_raster <- lapply(Sys.glob("*_f4570.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f4570_raster)) {
    f4570_raster[[i]] <- resample(f4570_raster[[i]], r1)
}

for(i in 1:length(f4570_raster)) {
    f4570_raster[[i]] [is.na(f4570_raster[[i]])]<-0
}

f4570_stack<-stack(f4570_raster)
f4570_layers<-overlay(f4570_stack, fun=sum)

jpeg(filename="f4570_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f4570_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f4570_layers, file="f4570_layers_52.tif", overwrite=T)
rm(f4570_raster, f4570_stack, f4570_layers)

#f6050
f6050_raster <- lapply(Sys.glob("*_f6050.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f6050_raster)) {
    f6050_raster[[i]] <- resample(f6050_raster[[i]], r1)
}

for(i in 1:length(f6050_raster)) {
    f6050_raster[[i]] [is.na(f6050_raster[[i]])]<-0
}

f6050_stack<-stack(f6050_raster)
f6050_layers<-overlay(f6050_stack, fun=sum)

jpeg(filename="f6050_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f6050_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f6050_layers, file="f6050_layers_52.tif", overwrite=T)
rm(f6050_raster, f6050_stack, f6050_layers)

#f6070
f6070_raster <- lapply(Sys.glob("*_f6070.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f6070_raster)) {
    f6070_raster[[i]] <- resample(f6070_raster[[i]], r1)
}

for(i in 1:length(f6070_raster)) {
    f6070_raster[[i]] [is.na(f6070_raster[[i]])]<-0
}

f6070_stack<-stack(f6070_raster)
f6070_layers<-overlay(f6070_stack, fun=sum)

jpeg(filename="f6070_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f6070_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f6070_layers, file="f6070_layers_52.tif", overwrite=T)
rm(f6070_raster, f6070_stack, f6070_layers)

#f8550
f8550_raster <- lapply(Sys.glob("*_f8550.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f8550_raster)) {
    f8550_raster[[i]] <- resample(f8550_raster[[i]], r1)
}

for(i in 1:length(f8550_raster)) {
    f8550_raster[[i]] [is.na(f8550_raster[[i]])]<-0
}

f8550_stack<-stack(f8550_raster)
f8550_layers<-overlay(f8550_stack, fun=sum)

jpeg(filename="f8550_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f8550_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f8550_layers, file="f8550_layers_52.tif", overwrite=T)
rm(f8550_raster, f8550_stack, f8550_layers)

#f8570
f8570_raster <- lapply(Sys.glob("*_f8570.asc"), raster)
r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)

for(i in 1:length(f8570_raster)) {
    f8570_raster[[i]] <- resample(f8570_raster[[i]], r1)
}

for(i in 1:length(f8570_raster)) {
    f8570_raster[[i]] [is.na(f8570_raster[[i]])]<-0
}

f8570_stack<-stack(f8570_raster)
f8570_layers<-overlay(f8570_stack, fun=sum)

jpeg(filename="f8570_plot.jpg", width = 4, height = 4, units = 'in', res=300, pointsize=6)
plot(f8570_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
dev.off()

writeRaster(f8570_layers, file="f8570_layers_52.tif", overwrite=T)
rm(f8570_raster, f8570_stack, f8570_layers)


#Additonal figures
pdf("niche_proposal.pdf")
par(mfrow=c(1,2))
par(mar=c(15, 5, 11, 5))
plot(current_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
title(main="Current time", line=1)
plot(f8570_layers, xlab="Longitude", ylab="Latitude")
map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)
title(main="RPC=80 year 2070", line=1)
dev.off()




######
#TEST#
######

#layer1 = raster("~/Dropbox/2016project/amphibianrastertest/Adelphobates quinquevittatus_current.asc")
#layer2 = raster("~/Dropbox/2016project/amphibianrastertest/Agalychnis callidryas_current.asc")
#layer3 = raster("~/Dropbox/2016project/amphibianrastertest/Allobates femoralis_current.asc")
#layer4 = raster("~/Dropbox/2016project/amphibianrastertest/Allobates talamancae_current.asc")

#dev.new()
#par(mfrow=c(2,2))
#image(layer1)
#image(layer2)
#image(layer3)
#image(layer4)

#r1 <- raster(nrows=5000, ncols=5000, xmn=-130, xmx=-30, ymn=-60, ymx=60)
#layer1b<-resample(layer1, r1)
#layer2b<-resample(layer2, r1)
#layer3b<-resample(layer3, r1)
#layer4b<-resample(layer4, r1)

#layer1b[is.na(layer1b)]<-0
#layer2b[is.na(layer2b)]<-0
#layer3b[is.na(layer3b)]<-0
#layer4b[is.na(layer4b)]<-0

#THESE TWO METHODS GIVE TEH SAME ANSWER
#stack<-stack(layer1b, layer2b, layer3b, layer4b)
#layers<-overlay(stack, fun=sum)
#dev.new()
#image(layers)
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)

#layersb<-sum(stack(layer1b, layer2b, layer3b, layer4b))
#dev.new()
#image(layersb)
#map(xlim = c(-130, 30), ylim = c(-60, 60), add=TRUE)

#get names of raster files
#list<-character()
#for(i in 1:length(raster_current)) {
#    number <- i
#    name<-paste("raster_current[[", number, "]]", sep="")
#    list<-append(list, name)
#}

#function to sum all raster layers
#we can use fun=sum in overlay
#fun=function(...) {return(sum(...))}

#e<-extent(-130, -30, -60, 60)
