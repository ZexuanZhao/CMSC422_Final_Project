args = commandArgs(trailingOnly=TRUE)

setwd(args[1])
wd<-getwd()

species<-basename(wd)

gps<-read.table(args[2])

#to get mid range lat for each species
max_lat <- max(gps$V4)
min_lat <- min(gps$V4)

abs_max_lat <- abs(max_lat)
abs_min_lat <- abs(min_lat)

length <- max_lat - min_lat

abs_mid_lat <- abs(mean(gps$V4))


setwd(args[3])
write.table(data.frame(species, length, abs_min_lat, abs_max_lat, abs_mid_lat), file = "additional_geo_dist_var.txt", sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)

		
			