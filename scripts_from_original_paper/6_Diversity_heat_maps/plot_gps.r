data<-read.table("")


library(plyr)
counts <- ddply(data, .(data$V5, data$V4), nrow)
names(counts) <- c("lon", "lat", "Freq")
#counts
counts<-counts[order(-counts$Freq),]
highcounts<-counts[c(1:150),]
lowcounts<-counts[c(151:38887),]
lowcounts<-lowcounts[order(counts$Freq),]

lowcounts$log <-log(lowcounts$Freq)

library(ggmap)
library(ggplot2)
world <- map_data("world")

#original code
#ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group)) +
#geom_point(data=counts, aes(x=lon, y=lat, colour=Freq, size=sqrt(Freq/pi))) +  #scale_colour_gradientn(colours = rev(heat.colors(12))) +
#  scale_size(guide = FALSE)
  
gg<-ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group)) +
geom_point(data=lowcounts, size = 1, pch = 15, alpha = 0.7, aes(x=lon, y=lat, colour=log))+ 
scale_colour_gradientn(colours = rev(topo.colors(12))) +
scale_size(guide = FALSE) + 
scale_y_continuous(breaks=(-4:4) * 30) + 
scale_x_continuous(breaks=(-6:6) * 30) + geom_point(data=highcounts, size = 2, pch = 15, alpha = 0.5, colour="red", aes(x=lon, y=lat))
plot(gg)



  
pdf("sampling_plot.pdf")
gg<-ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group)) +
+ geom_point(data=lowcounts, size = 1, pch = 15, aes(x=lon, y=lat, colour=log))+ 
+ scale_colour_gradientn(colours = rev(topo.colors(12))) +
+ scale_size(guide = FALSE) + 
+ scale_y_continuous(breaks=(-4:4) * 30) + 
+ scale_x_continuous(breaks=(-6:6) * 30) + geom_point(data=highcounts, size = 2, pch = 15, colour="red", aes(x=lon, y=lat))
plot(gg)
dev.off()




####OLD###
library(rworldmap)
library(rworldxtra)

newmap<-getMap(resolution="low")
#plot(newmap)
#points(data3$V5, data3$V4, col = "red", pch = 15, cex = 0.5)

worldmap <- ggplot(newmap, aes(x=lon, y=lat, group=group)) +
+   geom_path() +
+   scale_y_continuous(breaks=(-4:4) * 30) +
+   scale_x_continuous(breaks=(-6:6) * 30)


mapWorld <- borders("world", colour="black") # create a layer of borders
mp <- ggplot() +   mapWorld
mp
mp + scale_y_continuous(breaks=(-4:4) * 30) + scale_x_continuous(breaks=(-6:6) * 30)


mp <- ggplot(counts, aes(lon, lat, fill = Freq)) + geom_point(shape = 21) +   mapWorld
mp
mp + scale_y_continuous(breaks=(-4:4) * 30) + scale_x_continuous(breaks=(-6:6) * 30)
  
