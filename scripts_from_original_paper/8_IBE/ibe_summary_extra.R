setwd("")
wd<-getwd()
name <- basename(wd)

#table is: species, gene, n, pmtr, pmtp, dgr, dgp, der, dep, ger, gep
results<-read.table("ibe.txt", sep="\t")

n<-nrow(results)
n

#geo-env mantel test
temp<-results[,c(10,11)]
temp<-na.omit(temp)
ge.n <- nrow(temp)
ge.n
ge.p<-temp$V11
ge.x <- sum(ge.p <= 0.05)
ge.x
ge.prop <- ge.x/ge.n
ge.prop

binomial <- binom.test(ge.x, ge.n, 0.05, alternative = "g")
binomial
ge.sig <- binomial$p.value
ge.sig

ge.l <- vector()
for(i in 1:nrow(temp)) {
	
	if (temp$V11[i] <= 0.05) {
		ge.l <- c(ge.l, temp$V10[i])
		}}

ge.rlow<-min(ge.l)
ge.rhigh<-max(ge.l)
ge.rmed<-median(ge.l)
ge.rlow
ge.rhigh
ge.rmed
	
setwd("/Volumes/HD2/Tara2016/DATA")	
#OUTPUT DATA TO FILE
write.table(data.frame(name, ge.n, ge.x, ge.prop, ge.sig, ge.rlow, ge.rhigh, ge.rmed), file = "ibe_summary_extra.txt", sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE, append = TRUE)
