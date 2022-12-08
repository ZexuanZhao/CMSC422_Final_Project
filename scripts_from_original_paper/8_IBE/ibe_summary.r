setwd("")
wd<-getwd()
name <- basename(wd)

#table is: species, gene, n, pmtr, pmtp, dgr, dgp, der, dep, ger, gep
results<-read.table("ibe.txt", sep="\t", header=FALSE)

#removes rows with NA
#results<-na.omit(results)

n<-nrow(results)
n


#partial mantel test
temp<-results[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$V5
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop

binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

pmt.l <- vector()
for(i in 1:nrow(temp)) {
	
	if (temp$V5[i] <= 0.05) {
		pmt.l <- c(pmt.l, temp$V4[i])
		}}

pmt.rlow<-min(pmt.l)
pmt.rhigh<-max(pmt.l)
pmt.rmed<-median(pmt.l)
pmt.rlow
pmt.rhigh
pmt.rmed
	
	
#dna-geo mantel test
temp<-results[,c(6,7)]
temp<-na.omit(temp)
dg.n <- nrow(temp)
dg.n
dg.p<-temp$V7
dg.x <- sum(dg.p <= 0.05)
dg.x
dg.prop <- dg.x/dg.n
dg.prop

binomial <- binom.test(dg.x, dg.n, 0.05, alternative = "g")
binomial
dg.sig <- binomial$p.value
dg.sig

dg.l <- vector()
for(i in 1:nrow(temp)) {	
if (temp$V7[i] <= 0.05) {
		dg.l <- c(dg.l, temp$V6[i])
		} }
		
dg.rlow<-min(dg.l)
dg.rhigh<-max(dg.l)
dg.rmed<-median(dg.l)
dg.rlow
dg.rhigh
dg.rmed


#dna-env mantel test
temp<-results[,c(8,9)]
temp<-na.omit(temp)
de.n <- nrow(temp)
de.n
de.p<-temp$V9
de.x <- sum(de.p <= 0.05)
de.x
de.prop <- de.x/de.n
de.prop

binomial <- binom.test(de.x, de.n, 0.05, alternative = "g")
binomial
de.sig <- binomial$p.value
de.sig

de.l <- vector()
for(i in 1:nrow(temp)) {	
	if (temp$V9[i] <= 0.05) {
		de.l <- c(de.l, temp$V8[i])
		}}	

de.rlow<-min(de.l)
de.rhigh<-max(de.l)
de.rmed<-median(de.l)
de.rlow
de.rhigh
de.rmed


#OUTPUT DATA TO FILE
df <- data.frame(name, n, pmt.n, pmt.x, pmt.prop, pmt.sig, pmt.rlow, pmt.rhigh, pmt.rmed, dg.n, dg.x, dg.prop, dg.sig, dg.rlow, dg.rhigh, dg.rmed, de.n, de.x, de.prop, de.sig, de.rlow, de.rhigh, de.rmed)

df

write.table(df, file = paste(name,"_ibe_summary.txt", sep=""), sep = "\t", row.names=FALSE, quote=FALSE)

rm(list = ls())



###rarefaction
threshhold <- 10
data.10 <- subset(data, data[ , 3] > threshhold)
dim(data.10)
#partial mantel test
temp<-data.10[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 15
data.15 <- subset(data, data[ , 3] > threshhold)
dim(data.15)
#partial mantel test
temp<-data.15[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 20
data.20 <- subset(data, data[ , 3] > threshhold)
dim(data.20)
#partial mantel test
temp<-data.20[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 25
data.25 <- subset(data, data[ , 3] > threshhold)
dim(data.25)
#partial mantel test
temp<-data.25[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 30
data.30 <- subset(data, data[ , 3] > threshhold)
dim(data.30)
#partial mantel test
temp<-data.30[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 35
data.35 <- subset(data, data[ , 3] > threshhold)
dim(data.35)
#partial mantel test
temp<-data.35[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 40
data.40 <- subset(data, data[ , 3] > threshhold)
dim(data.40)
#partial mantel test
temp<-data.40[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 45
data.45 <- subset(data, data[ , 3] > threshhold)
dim(data.45)
#partial mantel test
temp<-data.45[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 50
data.50 <- subset(data, data[ , 3] > threshhold)
dim(data.50)
#partial mantel test
temp<-data.50[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 60
data.60 <- subset(data, data[ , 3] > threshhold)
dim(data.60)
#partial mantel test
temp<-data.60[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 70
data.70 <- subset(data, data[ , 3] > threshhold)
dim(data.70)
#partial mantel test
temp<-data.70[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 80
data.80 <- subset(data, data[ , 3] > threshhold)
dim(data.80)
#partial mantel test
temp<-data.80[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 90
data.90 <- subset(data, data[ , 3] > threshhold)
dim(data.90)
#partial mantel test
temp<-data.90[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 100
data.100 <- subset(data, data[ , 3] > threshhold)
dim(data.100)
#partial mantel test
temp<-data.100[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 200
data.200 <- subset(data, data[ , 3] > threshhold)
dim(data.200)
#partial mantel test
temp<-data.200[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 300
data.300 <- subset(data, data[ , 3] > threshhold)
dim(data.300)
#partial mantel test
temp<-data.300[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig

threshhold <- 400
data.400 <- subset(data, data[ , 3] > threshhold)
dim(data.400)
#partial mantel test
temp<-data.400[,c(4,5)]
temp<-na.omit(temp)
pmt.n <- nrow(temp)
pmt.n
pmt.p<-temp$pmtp
pmt.x <- sum(pmt.p <= 0.05)
pmt.x
pmt.prop <- pmt.x/pmt.n
pmt.prop
binomial <- binom.test(pmt.x, pmt.n, 0.05, alternative = "g")
binomial
pmt.sig <- binomial$p.value
pmt.sig