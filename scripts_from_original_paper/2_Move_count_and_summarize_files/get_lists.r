args = commandArgs(trailingOnly=TRUE)

setwd(args[1])
wd<-getwd()

data<-read.table(args[2], sep="\t")

family<-unique(data$V3[data$V3 != ""])
genus<-unique(data$V2[data$V2 != ""])

write.table(family, sep="\t", file="family.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
write.table(genus, sep="\t", file="genus.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)