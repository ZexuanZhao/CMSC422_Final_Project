######################################################
########LOOPS FOR RESAMPLING ALL DATA ################
########      AND JACKNIFING         #################
###########  by Anahi Espindola - Feb 2017 ###########
######################################################


###the data I am using is in the matrix aridData, which contains the coordinates and the variables I 
###want to use, as well as one column 'complex' that sets what localities belong to what species (complex)

levelscomplex=levels(aridData$complex) #creates a list of the different species (complexes) presnet in the data

#now this makes two loops that first take one of hte species out (the species we want to not consider) for our jacknifing 
#and then select the defined number of localities we want for each class (in our case, C = cryptic and NC = non-cryptic)
#in this example, we selected 141 samples for each class, and we created 100 resampled datasets of each


for (k in length(levelscomplex)) {
  aridSub=aridData[aridData$complex!=levelscomplex[k],]
  
  for (i in 1:100) {
    C=aridSub[aridSub$group=="C",]
    Csel=C[(sample(nrow(C), size=141, replace=T)),]
    NC=aridSub[aridSub$group=="NC",]
    NCsel=NC[(sample(nrow(NC), size=141, replace=T)),]
    aridSub=rbind(Csel,NCsel)
    
    rownames(aridSub) <- seq(length=nrow(aridSub))
    
    fitRFarid <- randomForest(group ~ bio1 + bio2 + bio3 + bio5 + bio7 + bio12 + bio15 + bio17 + taxon,
                              data=aridSub, importance=TRUE, ntree=500, replace=T)
    
    Contribution=fitRFarid$importance
    write.table(Contribution,file="fitRFPNWResamp141.csv", sep=",", append=T) # this saves the contributions of each variable into a file
    
    PredictionArid <- predict(fitRFarid)
    binded.dataRFarid=c()
    binded.dataRFarid=cbind(aridSub, PredictionArid) # bind initial data table with predicted values
    write.table(binded.dataRFarid,file="binded.dataPNW_RF_resampl141.csv", sep=",", append=T)
    
    ctRFarid <- table(binded.dataRFarid$group, PredictionArid)
    diag(prop.table(ctRFarid, 1))
    write.table(ctRFarid, file="confusionTableRF_PNWResampled141.csv", sep=",",append=T)
  
    Species=aridData[aridData$complex==levelscomplex[k],]
    PredictionNEWprob <- predict(fitRFarid,Species,type="prob") #calculates probability for each group
    PredictionNEW <- predict(fitRFarid,Species)
    binded.dataRFPNW=cbind(Species, PredictionNEWprob,PredictionNEW)
    binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
    write.table(binded.Predict[,c(20:21,33:38)], file="PredictOutspRFarid141.csv", sep=',', append=T) # here, the columns you select are the ones that have hte predicted probabilities and the species, and those change with the size of your matrix
    
    }
}


######################################################
########LOOPs FOR DOWNSAMPLING ALL DATA ##############
########      AND JACKNIFING         #################
###########  by Anahi Espindola - Feb 2017 ###########
######################################################

#note on this. There may be 1 million better ways to do this, but this one works too, even though it is missing some beauty

memory.limit(size=50000) # I needed to do this, because the data got relatively huge pretty quickly
binded.Predict=c()
for (i in 1:100) {
  PNWRF=c()
  fitRFPNW=c()
  PNWRF=PNWdata[PNWdata$complex!="Ascaphus.montanus.truei",] #here you take the excluded species away (this is for hte jacknifing)
  C=PNWRF[PNWRF$group=="C",]
  Csel=C[(sample(nrow(C), size=1459, replace=T)),] #the size here equals the number of observations of your minority class. Here you are selecting from the majority, based on that minority number (which will change, since at some point you'll take one of hte minority species away for jacknifing)
  NC=PNWRF[PNWRF$group=="NC",] #note that you're using all minority observations
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(group ~ bio1 + bio2 + bio3 + bio5 + bio7 + bio12 + bio15 + bio17 + taxon,
                           data=PNWRF, importance=TRUE, ntree=500, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="fitRFPNWResamp3000_final.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataPNW_RF_resampl3000_final.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$group, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableRF_PNWResampled3000_final.csv", sep=",",append=T) #here I was using exporting this table to make a graph after and do some summaries
  
  Ascaphus=PNWdata[PNWdata$complex=="Ascaphus.montanus.truei",] #here we want to put all the observations of that species in one place, so we can predict and see how well we're performing
  PredictionNEWprob <- predict(fitRFPNW,Ascaphus,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,Ascaphus)
  binded.dataRFPNW=cbind(Ascaphus, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictAscaphus.csv", sep=',', append=T)
}
x11()
print(plot(binded.dataRFPNW$PredictionNEW, main="Prediction Ascaphus", beside=F) )
dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
dev.off()
