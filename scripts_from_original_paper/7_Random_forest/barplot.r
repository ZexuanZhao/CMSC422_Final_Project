data<-read.table("barplot.txt", sep="\t")

#HIST
#put in order according to chart
cols<-c("black", "grey", "grey", "grey", "grey", "grey", "black")
pdf("global_rf.pdf")
par(mar=c(12, 4, 4, 2) + 1)
barplot(data$V3, names = data$V1, las=2,col=cols, main="Mean Decrease in Accuracy")
legend("topright", c("Extrinsic", "Intrinsic", "E x I"), col=c("black", "black", "grey"), pch=c(0, 15, 15))
dev.off()

data<-read.table("", sep="\t")
pdf("mammal_rf.pdf")
par(mar=c(6, 4, 4, 2) + 1)
cols<-c("grey", "grey", "grey", "grey", "grey", "grey", "black", "grey")
barplot(data$V2, names = data$V1, las=2, cex.names=0.75, col=cols, main="Mean Decrease in Accuracy")
legend("topright", c("E", "I", "E x I"), col=c("black", "grey", "black"), pch=c(0, 15, 15))
dev.off()


#BAR
cols<-c("black", "black", "black", "black", "darkgreen", "grey", "grey", "grey", "grey")
pdf("rf_plot.pdf")
par(mar=c(2, 12, 2, 0) + 1)
barplot(data$X2, names = data$X1, las=2,col=cols, main="Mean Decrease in Accuracy", horiz=TRUE, space=1, xlim=c(0,60), cex.axis=1.5, cex.names=1.5, cex.main=1.5)
legend(26,6, c("Geographic", "Intrinsic", "Land cover"), col=c("grey", "black", "darkgreen"), pch=c(15,15,15), cex=1.8)
dev.off()