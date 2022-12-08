library(ade4)
library(seqinr)
library(ape)

args = commandArgs(trailingOnly=TRUE)

#get IDs (accession numbers) from .afa file
mydata.fasta <-read.fasta(args[1])
#mydata.fasta
IDnames <- names(mydata.fasta)
IDnames <- substr(IDnames, 1,8)
IDnames

#get gps coordinates that match the ones from the fasta file we are analyzing
gps <- read.table(args[2], sep="\t")
gps <- gps[order(gps[,2]),]
gps <- gps[!duplicated(gps[,2]),]
#gps
mydata.coord <- gps[gps[,2] %in% IDnames,]
#mydata.coord

#get distance matrix for DNA data
mydata.dna <- read.dna(args[1], format="fasta")
#mydata.dna
mydata.dist <- dist.gene(mydata.dna)
#mydata.dist

# Geographical distance
mydata.geodist <- dist(mydata.coord[,3], mydata.coord[,4], method="euclidean", diag=TRUE, upper=TRUE)
#mydata.geodist

l1 <- length(mydata.dist)
l2 <- length(mydata.geodist)

# if statement to make sure matrices are same size (some gps coordinates are missing)
if (l1 == l2) {
    # Mantel test - IBD
    mydata.mantel <- mantel.randtest(m1=mydata.dist, m2=mydata.geodist, nrepet=1000)
    #mydata.mantel
    p<-mydata.mantel$p
} else {
    p<- ("invalid")
}

#plot(mydata.mantel, nclass=30)
print (p)
write(p, file = args[3], append = TRUE)
#write.table(data.frame(gps[1,1], p), file = args[3], sep = "\t", append = TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)

