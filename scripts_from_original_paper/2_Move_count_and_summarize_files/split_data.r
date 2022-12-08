#split data hierarchy that should be spit

setwd("")
data<-read.table("DATA_unique.txt", sep="\t")

a<-data[data$V4 == "Squamata", ]
a<-a[,1]
write.table(a, file="squamata_names.txt", col.names=FALSE, row.names=FALSE, quote=FALSE)

b<-data[data$V4 == "Testudines", ]
b<-b[,1]
write.table(b, file="testudines_names.txt", col.names=FALSE, row.names=FALSE, quote=FALSE)

c<-data[data$V4 == "Crocodylia", ]
c<-c[,1]
write.table(c, file="crocodylia_names.txt", col.names=FALSE, row.names=FALSE, quote=FALSE)