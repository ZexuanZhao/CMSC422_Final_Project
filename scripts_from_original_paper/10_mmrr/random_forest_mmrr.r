#data merges using merge.r script
#use file "rf_data_table.txt"
names(data)

data.g <- data[,c(-1,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-46,-47,-48,-49,-52,-54,-55,-56,-57,-58,-59,-60,-61,-63,-64,-65)]

data.e <- data[,c(-1,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-46,-47,-48,-49,-52,-54,-55,-56,-57,-58,-59,-60,-61,-62,-63,-64)]

library(IDPmisc)
data.g<-NaRV.omit(data.g)
data.e<-NaRV.omit(data.e)

for(i in 1:nrow(data.g)) {
  
  if (data.g$pgeo[i] > 0.05) {
    data.g$pgeo[i] <- 'no'
  }
  else {
    data.g$pgeo[i] <- 'yes'
  }
}

for(i in 1:nrow(data.e)) {
  
  if (data.e$penv[i] > 0.05) {
    data.e$penv[i] <- 'no'
  }
  else {
    data.e$penv[i] <- 'yes'
  }
}

data.g$pgeo <-as.factor(data.g$pgeo)
data.e$penv <-as.factor(data.e$penv)
lapply(data.e,class)

library(randomForest)

rf<-randomForest(pgeo ~ ., data=data.g, ntree=1000, importance=TRUE, nPerm=100)
rf
rf<-randomForest(penv ~ ., data=data.e, ntree=1000, importance=TRUE, nPerm=100)
rf

#resample so yes and no are even
table(data.g$pgeo)
for (i in 1:100) {
  Y=data.g[data.g$pgeo=="yes",]
  Ysel=Y[(sample(nrow(Y), size=1790, replace=T)),]
  N=data.g[data.g$pgeo=="no",]
  Nsel=N[(sample(nrow(N), size=1790, replace=T)),]
  dataSub=rbind(Ysel,Nsel)
  
  RF <- randomForest(pgeo ~ ., data=dataSub, importance=TRUE, ntree=1000, replace=T)
  
  imp <- data.frame(importance(RF))
  MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
  write.table(MDA,file="RF_mmrrgeo_mda.csv", sep=",", quote=FALSE, append=T) # this saves the contributions of each variable into a file
  
  error<-RF$confusion[,'class.error']
  write.table(error, file="RF_mmrrgeo_error.csv", sep=",",append=T, quote=FALSE, col.names=FALSE)
}
  

table(data.e$penv)
for (i in 1:100) {
  Y=data.e[data.e$penv=="yes",]
  Ysel=Y[(sample(nrow(Y), size=1403, replace=T)),]
  N=data.e[data.e$penv=="no",]
  Nsel=N[(sample(nrow(N), size=1403, replace=T)),]
  dataSub=rbind(Ysel,Nsel)
  
  RF <- randomForest(penv ~ ., data=dataSub, importance=TRUE, ntree=1000, replace=T)
  
  imp <- data.frame(importance(RF))
  MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
  write.table(MDA,file="RF_mmrrenv_mda.csv", sep=",", quote=FALSE, append=T) # this saves the contributions of each variable into a file
  
  error<-RF$confusion[,'class.error']
  write.table(error, file="RF_mmrrenv_error.csv", sep=",",append=T, quote=FALSE, col.names=FALSE)
}

##########
#ttest
#########


data.sig<-subset(data.g, pgeo == "yes")
data.not<-subset(data.g, pgeo == "no")

t.test(data.sig$area, data.not$area)
t.test(data.sig$abs_max_lat, data.not$abs_max_lat)
t.test(data.sig$abs_min_lat, data.not$abs_min_lat)
t.test(data.sig$abs_mid_lat, data.not$abs_mid_lat)
t.test(data.sig$length, data.not$length)
