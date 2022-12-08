library(randomForest)
setwd("")

#import the merged dataset (results file and variable file)
data<-read.table("data_table_ibe_rf.txt", sep="\t", header=TRUE)
dim(data)

#remove certain cases, for example where p = NA and area = NA
#data<-data[complete.cases(data[,c(4,36)]),]

#remove all NAs
#though this can be done in the randomForest command as well
data<-na.omit(data)
dim(data)

#to code p-value as yes or no
for(i in 1:nrow(data)) {
    
    if (data$p[i] > 0.5) {
        data$p[i] <- 'no'
    }
    else {
        data$p[i] <- 'yes'
    }
}


#to see how many categories are present for each varable
#rf can only handle up to 53 for categorical variables
apply(data, 2, function(data)length(unique(data)))



#prune data to remove variables with too many categories or nuisance variables
#though this can be done in the randomForest command as well
#FULL data
data <- data[,c(-1,-3,-5,-6,-7,-39,-40)]
dim(data)
#MAMMAL data
data <- data[,c(-1,-6,-7,-8,-9,-10,-11,-39)]
dim(data)

#change p to factor so rf will run in classification mode
data$p <-as.factor(data$p)



#run random forest
rf<-randomForest(pmtp ~ ., data=data.n, ntree=1000, importance=TRUE, nPerm=100)
summary(rf)
imp <- data.frame(importance(rf))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA
#get variables in order of importance when run in regression mode
#INC<-imp[order(imp$IncNodePurity,decreasing = T),]
#INC
#MSE seems like the most reasonable to use
MSE<-imp[order(imp$X.IncMS,decreasing = T),]
MSE
write.table(MSE, file="rf.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)



#remove rows with values that might be influencing results (i.e. n<10)
threshhold <- 10
data <- subset(data, data[ , 3] > threshhold)
dim(data)
#run random forest
rf_10<-randomForest(p ~ ., data=data, ntree=1000, importance=TRUE, nPerm=100)
imp <- data.frame(importance(rf_10))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA
#get variables in order of importance when run in regression mode
#INC<-imp[order(imp$IncNodePurity,decreasing = T),]
#INC
#MSE seems like the most reasonable to use
MSE<-imp[order(imp$X.IncMS,decreasing = T),]
MSE
write.table(MSE, file="rf_10.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)


#remove rows with values that might be influencing results (i.e. n<10)
threshhold <- 15
data <- subset(data, data[ , 3] > threshhold)
dim(data)
#run random forest
rf_15<-randomForest(p ~ ., data=data, ntree=1000, importance=TRUE, nPerm=100)
imp <- data.frame(importance(rf_15))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA
#get variables in order of importance when run in regression mode
#INC<-imp[order(imp$IncNodePurity,decreasing = T),]
#INC
#MSE seems like the most reasonable to use
MSE<-imp[order(imp$X.IncMS,decreasing = T),]
MSE
write.table(MSE, file="rf_15.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)


#remove rows with values that might be influencing results (i.e. n<10)
threshhold <- 20
data <- subset(data, data[ , 3] > threshhold)
dim(data)
#run random forest
rf_20<-randomForest(p ~ ., data=data, ntree=1000, importance=TRUE, nPerm=100)
imp <- data.frame(importance(rf_20))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA
#get variables in order of importance when run in regression mode
#INC<-imp[order(imp$IncNodePurity,decreasing = T),]
#INC
#MSE seems like the most reasonable to use
MSE<-imp[order(imp$X.IncMS,decreasing = T),]
MSE
write.table(MSE, file="rf_20.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)



#remove rows with values that might be influencing results (i.e. n<10)
threshhold <- 30
data <- subset(data, data[ , 3] > threshhold)
dim(data)
#run random forest
rf_30<-randomForest(p ~ ., data=data, ntree=1000, importance=TRUE, nPerm=100)
imp <- data.frame(importance(rf_30))
MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA
#get variables in order of importance when run in regression mode
#INC<-imp[order(imp$IncNodePurity,decreasing = T),]
#INC
#MSE seems like the most reasonable to use
MSE<-imp[order(imp$X.IncMS,decreasing = T),]
MSE
write.table(MSE, file="rf_30.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)







#make plots of variables importance, n.var is how many variables you want on plot
varImpPlot(rf, sort=TRUE, n.var=6, main="Variable Importance")


##############################
#to get p values for variables
#THIS HAS NOT WORKED YET - SEEMS TO TAKE FOREVER
library(rfPermute)
rfp<-rfPermute(p ~ ., data=data, ntree=1000, importance=TRUE, nPerm=100)

#to see plots of variable dependence
#change variable to get that plot
partialPlot(rf, data, gene, data$p)
partialPlot(rf, data, n, data$p)

par(mfrow=c(3,2))
partialPlot(rf, data, max_lon, data$p)
partialPlot(rf, data, min_lon, data$p)
partialPlot(rf, data, min_lat, data$p)
partialPlot(rf, data, max_lat, data$p)
partialPlot(rf, data, X210, data$p)
partialPlot(rf, data, area, data$p)

##########


rownames(MDA) <- c("X11"="Post-flooding or irrigated croplands (or aquatic)",
"X14"="Rainfed croplands",
"X20"="	Mosaic cropland (50-70%) / vegetation (grassland/shrubland/forest) (20-50%)",
"X30"="	Mosaic vegetation (grassland/shrubland/forest) (50-70%) / cropland (20-50%)",
"X40"="Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)",
"X50"="Closed (>40%) broadleaved deciduous forest (>5m)",
"X60"="Open (15-40%) broadleaved deciduous forest/woodland (>5m)",
"X70"="Closed (>40%) needleleaved evergreen forest (>5m)",
"X90"="Open (15-40%) needleleaved deciduous or evergreen forest (>5m)",
"X100"="Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)",
"X110"="Mosaic forest or shrubland (50-70%) / grassland (20-50%)",
"X120"="Mosaic grassland (50-70%) / forest or shrubland (20-50%)",
"X130"="Closed to open (>15%) (broadleaved or needleleaved, evergreen or deciduous) shrubland (<5m)",
"X140"="Closed to open (>15%) herbaceous vegetation (grassland, savannas or lichens/mosses)",
"X150"="Sparse (<15%) vegetation",
"X160"="Closed to open (>15%) broadleaved forest regularly flooded (semi-permanently or temporarily) - Fresh or brackish water",
"X170"="Closed (>40%) broadleaved forest or shrubland permanently flooded - Saline or brackish water",
"X180"="Closed to open (>15%) grassland or woody vegetation on regularly flooded or waterlogged soil - Fresh, brackish or saline water",
"X190"="Artificial surfaces and associated areas (Urban areas >50%)",
"X200"="Bare areas",
"X210"="Water bodies",
"X220"="Permanent snow and ice",
"X230"="No data (burnt areas, clouds,…)")


#################
#TO DO MODEL VALIDATION???

#split data into training and validation sets to assess how well the model performs on this dataset
sample<-sample(2, nrow(data), replace=T, prob =c(0.7, 0.3))
train.data <- data[sample==1,]
val.data <- data[sample==2,]

test.rf <-randomForest(p~.,data=train.data, tree=1000, importance=TRUE, test=val.data)
train.rf<-randomForest(p ~ ., data=train.data, ntree=1000, importance=TRUE)
#to see at how many trees the error rate stops decreasing
plot(train.rf)
plot(test.rf)


#DO SOMETHING HERE WITH TRAINING AND VALIDATION SETS HERE???

#to get AUC value
library(pROC)
multiclass.roc(data$p, predict(rf, data))
multiclass.roc(val.data$p, predict(rf, val.data))
multiclass.roc(train.data$p, predict(rf, train.data))


#########

str(data)


class(data$min_lon)
class(data$X210)
class(data$min_lat)
class(data$genus)
class(data$X130)
class(data$family)
class(data$max_lat)
class(data$environment)
class(data$X30)
class(data$X70)
class(data$X110)
class(data$order)
class(data$phylum)
class(data$kingdom)
class(data$metobolism)
class(data$X11)
class(data$X40)
class(data$X60)
class(data$X90)
class(data$X150)
class(data$X160)
class(data$X170)
class(data$X180)
class(data$X200)
class(data$X220)
class(data$X230)
class(data$X20)
class(data$X140)
class(data$X190)
class(data$class)
class(data$max_lon)
class(data$X120)
class(data$n)
class(data$species)
class(data$gene)
class(data$X14)
class(data$X50)
class(data$X100)
class(data$area)
class(data$p)