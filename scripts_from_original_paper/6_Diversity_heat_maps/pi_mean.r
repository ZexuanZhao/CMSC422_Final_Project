#OLD
setwd("")
data_n1<-read.table("pi.txt", sep="\t")
dim(data_n1)
#head(data_n1)
data_n2<-read.table("ibe.txt", sep="\t")
data_n2<-data_n2[,c(1,2,3,5)]
dim(data_n2)
#head(data_n2)

data_n<-merge(data_n1, data_n2, by = c("V1", "V2"), all=TRUE)
dim(data_n)
data_n<-na.omit(data_n)
dim(data_n)
head(data_n)
mean(data_n[,3])
mean(data_n[,4])


####NEW


#table from ibd_summary_script
data_n<-na.omit(data4)
dim(data_n)
mean(data_n[,3])
min(data_n[,3])
max(data_n[,3])
mean(data_n[,12])
min(data_n[,12])
max(data_n[,12])
mean(data_n[,13])
min(data_n[,13])
max(data_n[,13])
length(unique(data_n$V1))



mean(temp[,2])
min(temp[,2])
max(temp[,2])
mean(temp[,5])
min(temp[,5])
max(temp[,5])
mean(temp[,6])
min(temp[,6])
max(temp[,6])
length(unique(temp$V1))