library(gdistance)
library(ape)
library(adegent)


#genetic distance matrix same as ibd.r script
#TRY OTHER DISTANCE METRICS???
mydata.dna <- read.dna("~/Dropbox/2016project/Pinus ponderosa copy/LEA-like.afa", format="fasta")
mydata.dist <- dist.gene(mydata.dna)

#get gpspoints for distance matrix
#get IDs (accession numbers) from .afa file
mydata.fasta <-read.fasta("~/Dropbox/2016project/Pinus ponderosa copy/LEA-like.afa")
#mydata.fasta
IDnames <- names(mydata.fasta)
IDnames <- substr(IDnames, 1,8)
IDnames
gps<-read.table("~/Dropbox/2016project/Pinus ponderosa copy/Pinus ponderosa.txt", sep="\t")
mydata.coord <- gps[gps[,2] %in% IDnames,]
gpspoints<- mydata.coord[c(4,3)]
points<-data.matrix(gpspoints)

#loads niche model as raster file
ascdata<-raster("~/Dropbox/2016project/Pinus ponderosa copy/Pinus ponderosa copy.asc")
#plot(ascdata)
#ascdata

#make assymmetric transition matrix for each cell to each connecting cell (8 total)
ncf <- function(x) max(x) - x[1] + x[2]
tr <- transition(ascdata, ncf, directions=8, symm=FALSE)

#distortion correction because grid cells are not the same at pole vs equator
trC <- geoCorrection(tr, type="c", multpl=FALSE, scl=TRUE)
#correction to weigh probability of getting from one cell to another because they are different sizes
trR <- geoCorrection(tr, type="r", multpl=FALSE, scl=TRUE)

#least-cost distance matrix based on niche model raster file
#DO THESE VALUES CORRECTLY MEASURE WHAT WE ARE LOOKING FOR? SHOULD THEY BE THE OPPOSITE SO HIGH VALUES ARE GOOD AND LOW VALUES ARE BAD?
cost<-costDistance(trC, points)
cost.dist<-as.dist(cost)

l1 <- length(mydata.dist)
l2 <- length(cost.dist)

# if statement to make sure matrices are same size (some gps coordinates are missing)
if (l1 == l2) {
    # Mantel test - IBD
    mydata.mantel <- mantel.randtest(m1=mydata.dist, m2=cost.dist, nrepet=1000)
    #mydata.mantel
    p<-mydata.mantel$p
} else {
    p<- ("invalid")
}



############

library(PopGenReport)
popgen <- wassermann(gen.mat=mydata.dist, cost.mats = , eucl.mat=mydata.geodist, plot = TRUE, nperm=999)
