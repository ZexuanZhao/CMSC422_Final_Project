#invert ibd trait table

data<-read.table("~/Desktop/Tara2016/DATA/Arthropods/results_inverts.txt", sep="\t")
dim(data)
data<-data[complete.cases(data[,4]),]
dim(data)

traits<-read.csv("~/Desktop/Tara2016/DATA/Arthropods/invert_traits/InvertTraitsTable_v1.csv")
dim(traits)

df<-merge(data, traits, by.x="V1", by.y="Taxon", all.x=TRUE, all.y=FALSE)
dim(df)

#remove rows that do not have trait data
df<-df[complete.cases(df[,6]),]
#remove unecessary fields
df <- df[,-c(5,6,7,8,10,14,16,17,52,56,63,68,71,86,92,98,106,110,116,131)]
dim(df)

write.table(df, file="data_table_ibd_rf_traits.txt", quote = FALSE, sep = "\t", row.names=FALSE)

##############
#rf analysis##
##############

setwd("~/Desktop/Tara2016/DATA/Arthropods/invert_traits")
d<-read.csv("data_table_ibd_rf_traits.csv", na.strings=c("", " "))
dim(d)

#to code p-value as yes or no
for(i in 1:nrow(d)) {
    
    if (d$p[i] > 0.5) {
        d$p[i] <- 'no'
    }
    else {
        d$p[i] <- 'yes'
    }
}


#to see how many categories are present for each varable
#rf can only handle up to 53 for categorical variables
apply(d, 2, function(d)length(unique(d)))

#remove species name column
d <- d[,c(-1)]

library(randomForest)

#change p to factor so rf will run in classification mode
d$p <-as.factor(d$p)

#run random forest
rf<-randomForest(p ~ ., data=d, ntree=1000, importance=TRUE, nPerm=100)
imp <- data.frame(importance(rf))

MDA<-imp[order(imp$MeanDecreaseAccuracy,decreasing = T),]
MDA

############################### rf does not handle missing data well
#remove rows with missing data
d1<-na.omit(d)
dim(d1)

#remove columns with missing data
d2<-d[colSums(is.na(d)) < 1]
dim(d2)