rm(list = ls())
setwd("")
wd<-getwd()
name <- basename(wd)

###table is: species, gene, n, r,c,t,p,f,fp, cgeo,tgeo,pgeo, cenv,tenv,penv
results<-read.table("MMRR.txt", sep="\t")

###removes rows with NA - MMRR doesn't with n<3
results<-na.omit(results)

n<-nrow(results)
n


###r-squared
rmean <-mean(results$V4, na.omit=T)
rmin <-min(results$V4, na.omit=T)
rmax <-max(results$V4, na.omit=T)
rmean
rmin
rmax


###prop sig for overall
ap.sum <- sum(results$V7 <=0.05)
ap.prop <- ap.sum/n
ap.prop

###reg for those that are sig for geo
a.p <- vector()
for(i in 1:nrow(results)) {
	if (results$V7[i] <= 0.05) {
		a.p <- c(a.p, results$V5[i])
		}}
amean <-mean(a.p)
amin <-min(a.p)
amax <-max(a.p)
amean
amin
amax

binomiala <- binom.test(ap.sum, n, 0.05, alternative = "g")
binomiala
ap.sig <- binomiala$p.value
ap.sig

###prop sig for geo
gp.sum <- sum(results$V12 <=0.05)
gp.prop <- gp.sum/n
gp.prop

###reg for those that are sig for geo
g.p <- vector()
for(i in 1:nrow(results)) {
	if (results$V12[i] <= 0.05) {
		g.p <- c(g.p, results$V10[i])
		}}
gmean <-mean(g.p)
gmin <-min(g.p)
gmax <-max(g.p)
gmean
gmin
gmax

binomialg <- binom.test(gp.sum, n, 0.05, alternative = "g")
binomialg
gp.sig <- binomialg$p.value
gp.sig


###prop sig for env
ep.sum <- sum(results$V15 <=0.05)
ep.prop <- ep.sum/n
ep.prop

###reg for those that are sig for env
e.p <- vector()
for(i in 1:nrow(results)) {
	if (results$V15[i] <= 0.05) {
		e.p <- c(e.p, results$V13[i])
		}}
emean <-mean(e.p)
emin <-min(e.p)
emax <-max(e.p)
emean
emin
emax

binomiale <- binom.test(ep.sum, n, 0.05, alternative = "g")
binomiale
ep.sig <- binomiale$p.value
ep.sig
	

###OUTPUT DATA TO FILE
df <- data.frame(name, n, rmean, rmin, rmax, ap.prop, ap.sig, amean, amin, amax, gp.prop, gp.sig, gmean, gmin, gmax, ep.prop, ep.sig, emean, emin, emax)

df

write.table(df, file = paste(name,"_mmrr_summary.txt", sep=""), sep = "\t", row.names=FALSE, quote=FALSE)

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