setwd("")

#import the merged dataset (results file and variable file)
data<-read.table("ibe_rf_data.txt", sep="\t", header=TRUE)
dim(data)

#remove all NAs
data.n<-na.omit(data)
dim(data.n)

#to code p-values as yes or no
for(i in 1:nrow(data.n)) {
    
    if (data.n$pmtp[i] > 0.05) {
        data.n$pmtp[i] <- 'no'
    }
    else {
        data.n$pmtp[i] <- 'yes'
    }
}s

data.sig<-subset(data.n, pmtp = 'yes')
data.not<-subset(data.n, pmtp = 'no')

data.sig<-data.n[data.n$pmtp == 'yes', ]
data.not<-data.n[data.n$pmtp == 'no', ]

par(mfrow=c(3,3))
hist(data.sig$area)
hist(data.sig$abs_max_lat)
hist(data.sig$abs_min_lat)
hist(data.sig$abs_mid_lat)
hist(data.sig$length_lat)

par(mfrow=c(5,5))
hist(data.sig$area)
hist(data.sig$abs_max_lat)
hist(data.sig$abs_min_lat)
hist(data.sig$abs_mid_lat)
hist(data.sig$length_lat)
hist(data.not$area)
hist(data.not$abs_max_lat)
hist(data.not$abs_min_lat)
hist(data.not$abs_mid_lat)
hist(data.not$length_lat)

mean(data.sig$area)
mean(data.sig$abs_max_lat)
mean(data.sig$abs_min_lat)
mean(data.sig$abs_mid_lat)
mean(data.sig$length_lat)

mean(data.not$area)
mean(data.not$abs_max_lat)
mean(data.not$abs_min_lat)
mean(data.not$abs_mid_lat)
mean(data.not$length_lat)

t.test(data.sig$area, data.not$area)
t.test(data.sig$abs_max_lat, data.not$abs_max_lat)
t.test(data.sig$abs_min_lat, data.not$abs_min_lat)
t.test(data.sig$abs_mid_lat, data.not$abs_mid_lat)
t.test(data.sig$length_lat, data.not$length_lat)

wilcox.test(data.sig$area, data.not$area)
wilcox.test(data.sig$abs_max_lat, data.not$abs_max_lat)
wilcox.test(data.sig$abs_min_lat, data.not$abs_min_lat)
wilcox.test(data.sig$abs_mid_lat, data.not$abs_mid_lat)
wilcox.test(data.sig$length_lat, data.not$length_lat)



#############################################################
area <- lm(data.sig$pmtr ~ data.sig$area, data=data.sig)
summary(area) 
maxlat <- lm(data.sig$pmtr ~ data.sig$max_lat, data=data.sig)
summary(maxlat) 
minlat <- lm(data.sig$pmtr ~ data.sig$min_lat, data=data.sig)
summary(minlat) 
midlat <- lm(data.sig$pmtr ~ data.sig$latmid, data=data.sig)
summary(midlat) 
llat <- lm(data.sig$pmtr ~ data.sig$latlength, data=data.sig)
summary(llat) 


