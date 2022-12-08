library(randomForest)
setwd("")

#import the merged dataset (results file and variable file)
data<-read.table("data_table_ibe_rf.txt", sep="\t", header=TRUE)
dim(data)

#RF on partial Mantel test
#FULL data - remove variables we don't need or are too large to use
names(data)
data.n <- data[,c(-1,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-46,-47,-48,-49,-52)]
data.n<-na.omit(data.n)
dim(data.n)

#to code p-values as yes or no
for(i in 1:nrow(data.n)) {
    
    if (data.n$pmtp[i] > 0.05) {
        data.n$pmtp[i] <- 'no'
    }
    else {
        data.n$pmtp[i] <- 'yes'
    }
}

#change p to factor so rf will run in classification mode
data.n$pmtp <-as.factor(data.n$pmtp)

#run random forest
rf<-randomForest(pmtp ~ ., data=data.n, ntree=1000, importance=TRUE, nPerm=100)
rf
imp <- data.frame(importance(rf))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
write.table(MDA, file="rf.txt", quote = FALSE, sep = "\t")

#remove rows with values that might be influencing results (i.e. n<10)
threshhold <- 10
data.10 <- subset(data.n, data.n[ , 2] > threshhold)
dim(data.10)

#run random forest
rf.10<-randomForest(pmtp ~ ., data=data.10, ntree=1000, importance=TRUE, nPerm=100)
rf.10
imp <- data.frame(importance(rf.10))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
write.table(MDA, file="rf_10.txt", quote = FALSE, sep = "\t")


#remove rows with values that might be influencing results (i.e. n<10)
threshhold <- 20
data.20 <- subset(data.n, data.n[ , 2] > threshhold)
dim(data.20)

#run random forest
rf.20<-randomForest(pmtp ~ ., data=data.20, ntree=1000, importance=TRUE, nPerm=100)
rf.20
imp <- data.frame(importance(rf.20))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
write.table(MDA, file="rf_20.txt", quote = FALSE, sep = "\t")

########subsampling to test for biases


#resample so yes and no are even
table(data.20$pmtp)
for (i in 1:100) {
    Y=data.20[data.20$pmtp=="yes",]
    Ysel=Y[(sample(nrow(Y), size=696, replace=T)),]
    N=data.20[data.20$pmtp=="no",]
    Nsel=N[(sample(nrow(N), size=696, replace=T)),]
    dataSub=rbind(Ysel,Nsel)
  
    RF <- randomForest(pmtp ~ ., data=dataSub, importance=TRUE, ntree=1000, replace=T)
	
	imp <- data.frame(importance(RF))
	MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
    write.table(MDA,file="RF-resample_20.csv", sep=",", quote=FALSE, append=T) # this saves the contributions of each variable into a file

	error<-RF$confusion[,'class.error']
	write.table(error, file="error_20.csv", sep=",",append=T, quote=FALSE, col.names=FALSE)
}



#resample data so latitude is even
#hist(data.10$abs_mid_lat)
sum(data.10$abs_mid_lat < 23.5)
sum(data.10$abs_mid_lat > 23.5)
#hist(data.10$abs_min_lat)
sum(data.20$abs_min_lat < 23.5)
sum(data.20$abs_min_lat > 23.5)

for (i in 1:100) {
    Temp=data.10[data.10$abs_mid_lat > 23.5,]
    Tempsel=Temp[(sample(nrow(Temp), size=1750, replace=T)),]
    Trop=data.10[data.10$abs_mid_lat < 23.5,]
    Tropsel=Trop[(sample(nrow(Trop), size=1750, replace=T)),]
    latSub=rbind(Tempsel,Tropsel)
    
    RF <- randomForest(pmtp ~ ., data=latSub, importance=TRUE, ntree=1000, replace=T)
	
	imp <- data.frame(importance(RF))
	MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
    write.table(MDA,file="RF-resample_midlat_10.csv", sep=",", quote=FALSE, append=T) # this saves the contributions of each variable into a file

	error<-RF$confusion[,'class.error']
	write.table(error, file="error_midlat_10.csv", sep=",",append=T, quote=FALSE, col.names=FALSE)
}


#summarise subsampling results

e<-read.csv("", header=FALSE)
head(e)
y<-e[e$V1=="yes",]
n<-e[e$V1=="no",]
mean(y$V2)
min(y$V2)
max(y$V2)
mean(n$V2)
min(n$V2)
max(n$V2)


i<-read.csv("")
head(i)
aggregate(MeanDecreaseAccuracy~X, data=i, mean)
sapply(split(i$MeanDecreaseAccuracy, i$X), mean) 

#
=AVERAGEIF(A2:A3600, "n", D2:D3600)
=AVERAGEIF(A2:A3600, "length", D2:D3600)
=AVERAGEIF(A2:A3600, "abs_max_lat", D2:D3600)
=AVERAGEIF(A2:A3600, "abs_mid_lat", D2:D3600)
=AVERAGEIF(A2:A3600, "Rainfed.croplands", D2:D3600)
=AVERAGEIF(A2:A3600, "Mosaic.cropland..50.70.....vegetation..grassland.shrubland.forest...20.50..", D2:D3600)
=AVERAGEIF(A2:A3600, "abs_min_lat", D2:D3600)
=AVERAGEIF(A2:A3600, "Mosaic.vegetation..grassland.shrubland.forest...50.70.....cropland..20.50..", D2:D3600)
=AVERAGEIF(A2:A3600, "area", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed...40...broadleaved.deciduous.forest...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed.to.open...15...broadleaved.evergreen.or.semi.deciduous.forest...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "Water.bodies", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed.to.open...15...herbaceous.vegetation..grassland..savannas.or.lichens.mosses.", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed.to.open...15...mixed.broadleaved.and.needleleaved.forest...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed.to.open...15....broadleaved.or.needleleaved..evergreen.or.deciduous..shrubland...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "Mosaic.grassland..50.70.....forest.or.shrubland..20.50..", D2:D3600)
=AVERAGEIF(A2:A3600, "Sparse...15...vegetation", D2:D3600)
=AVERAGEIF(A2:A3600, "Post.flooding.or.irrigated.croplands..or.aquatic.", D2:D3600)
=AVERAGEIF(A2:A3600, "Mosaic.forest.or.shrubland..50.70.....grassland..20.50..", D2:D3600)
=AVERAGEIF(A2:A3600, "Phylum", D2:D3600)
=AVERAGEIF(A2:A3600, "Artificial.surfaces.and.associated.areas..Urban.areas..50..", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed...40...needleleaved.evergreen.forest...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "Open..15.40...needleleaved.deciduous.or.evergreen.forest...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "Open..15.40...broadleaved.deciduous.forest.woodland...5m.", D2:D3600)
=AVERAGEIF(A2:A3600, "habit", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed.to.open...15...broadleaved.forest.regularly.flooded..semi.permanently.or.temporarily....Fresh.or.brackish.water", D2:D3600)
=AVERAGEIF(A2:A3600, "Bare.areas", D2:D3600)
=AVERAGEIF(A2:A3600, "gene", D2:D3600)
=AVERAGEIF(A2:A3600, "metabolism", D2:D3600)
=AVERAGEIF(A2:A3600, "Class", D2:D3600)
=AVERAGEIF(A2:A3600, "Kingdom", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed...40...broadleaved.forest.or.shrubland.permanently.flooded...Saline.or.brackish.water", D2:D3600)
=AVERAGEIF(A2:A3600, "Closed.to.open...15...grassland.or.woody.vegetation.on.regularly.flooded.or.waterlogged.soil...Fresh..brackish.or.saline.water", D2:D3600)
=AVERAGEIF(A2:A3600, "No.data..burnt.areas..clouds...", D2:D3600)
=AVERAGEIF(A2:A3600, "Permanent.snow.and.ice", D2:D3600)

=AVERAGE(D2:D101)
=AVERAGE(D102:D201)
=AVERAGE(D202:D301)
=AVERAGE(D302:D401)
=AVERAGE(D402:D501)
=AVERAGE(D502:D601)
=AVERAGE(D602:D701)
=AVERAGE(D702:D801)
=AVERAGE(D802:D901)
=AVERAGE(D902:D1001)
=AVERAGE(D1002:D1101)
=AVERAGE(D1102:D1201)
=AVERAGE(D1202:D1301)
=AVERAGE(D1302:D1401)
=AVERAGE(D1402:D1501)
=AVERAGE(D1502:D1601)
=AVERAGE(D1602:D1701)
=AVERAGE(D1702:D1801)
=AVERAGE(D1802:D1901)
=AVERAGE(D1902:D2001)
=AVERAGE(D2002:D2101)
=AVERAGE(D2102:D2201)
=AVERAGE(D2202:D2301)
=AVERAGE(D2302:D2401)
=AVERAGE(D2402:D2501)
=AVERAGE(D2502:D2601)
=AVERAGE(D2602:D2701)
=AVERAGE(D2702:D2801)
=AVERAGE(D2802:D2901)
=AVERAGE(D2902:D3001)
=AVERAGE(D3002:D3101)
=AVERAGE(D3102:D3201)
=AVERAGE(D3202:D3301)
=AVERAGE(D3302:D3401)

=min(D2:D101)
=min(D102:D201)
=min(D202:D301)
=min(D302:D401)
=min(D402:D501)
=min(D502:D601)
=min(D602:D701)
=min(D702:D801)
=min(D802:D901)
=min(D902:D1001)
=min(D1002:D1101)
=min(D1102:D1201)
=min(D1202:D1301)
=min(D1302:D1401)
=min(D1402:D1501)
=min(D1502:D1601)
=min(D1602:D1701)
=min(D1702:D1801)
=min(D1802:D1901)
=min(D1902:D2001)
=min(D2002:D2101)
=min(D2102:D2201)
=min(D2202:D2301)
=min(D2302:D2401)
=min(D2402:D2501)
=min(D2502:D2601)
=min(D2602:D2701)
=min(D2702:D2801)
=min(D2802:D2901)
=min(D2902:D3001)
=min(D3002:D3101)
=min(D3102:D3201)
=min(D3202:D3301)
=min(D3302:D3401)

=max(D2:D101)
=max(D102:D201)
=max(D202:D301)
=max(D302:D401)
=max(D402:D501)
=max(D502:D601)
=max(D602:D701)
=max(D702:D801)
=max(D802:D901)
=max(D902:D1001)
=max(D1002:D1101)
=max(D1102:D1201)
=max(D1202:D1301)
=max(D1302:D1401)
=max(D1402:D1501)
=max(D1502:D1601)
=max(D1602:D1701)
=max(D1702:D1801)
=max(D1802:D1901)
=max(D1902:D2001)
=max(D2002:D2101)
=max(D2102:D2201)
=max(D2202:D2301)
=max(D2302:D2401)
=max(D2402:D2501)
=max(D2502:D2601)
=max(D2602:D2701)
=max(D2702:D2801)
=max(D2802:D2901)
=max(D2902:D3001)
=max(D3002:D3101)
=max(D3102:D3201)
=max(D3202:D3301)
=max(D3302:D3401)
