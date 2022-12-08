args = commandArgs(trailingOnly=TRUE)

setwd(args[1])

gps<-read.table(args[2])

setwd("/Volumes/HD2/Tara2016/DATA")
write.table(gps, file="gps_all.txt", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)




		
			