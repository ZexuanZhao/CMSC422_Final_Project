setwd("")
table<-read.table("ibe_rf_data.txt", sep="\t")
list<-table[,1]
write.table(list, file="ibe_names.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
