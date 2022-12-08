library(pegas)
library(tools)

args = commandArgs(trailingOnly=TRUE)
setwd(args[1])
wd<-getwd()
species <- basename(wd)

data <- read.dna(args[2], format="fasta")
gene <- file_path_sans_ext(args[2])
pi <- nuc.div(data)
n<-length(labels(data))
bp<-length(data)/n

setwd(args[3])
write.table(data.frame(species, gene, n, bp, pi), file = "pi.txt", sep = "\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)