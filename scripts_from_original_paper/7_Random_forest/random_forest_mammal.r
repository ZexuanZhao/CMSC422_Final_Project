library(randomForest)
setwd("")

#import the merged dataset (results file and variable file)
data<-read.table("ibe_rf_data_mammals.txt", sep="\t", header=TRUE)
dim(data)

traits<-read.table("~/Desktop/remammalrf/PanTHERIA_1-0_WR93_Aug2008.txt", sep="\t", header=TRUE)

full_data<-merge(data, traits, by.x="species", by.y="MSW93_Binomial", all.x=TRUE, all.y=FALSE)
dim(full_data)
data_m<-full_data[grep("Mammalia", full_data$class),]
dim(data_m)

#to see how many categories are present for each varable
#rf can only handle up to 53 for categorical variables
apply(data_m, 2, function(data_m)length(unique(data_m)))
apply(data_rf, 2, function(data_rf)length(unique(data_rf)))

#MAMMAL data
data_rf <- data_m[,c(-1,-4,-6,-7,-8,-9,-10,-11,-13,-14,-51,-52,-53,-54,-85)]
dim(data_rf)

data_rf<-na.omit(data_rf)
dim(data_rf)

#to code p-values as yes or no
data_rf$pmtp <-as.numeric(data_rf$pmtp)
for(i in 1:nrow(data_rf)) {
    
    if (data_rf$pmtp[i] > 0.05) {
        data_rf$pmtp[i] <- 'no'
    }
    else {
        data_rf$pmtp[i] <- 'yes'
    }
}
data_rf$pmtp <-as.factor(data_rf$pmtp)

#run random forest
rf<-randomForest(pmtp ~ ., data=data_rf, ntree=1000, importance=TRUE, nPerm=100)

imp <- data.frame(importance(rf))

MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA