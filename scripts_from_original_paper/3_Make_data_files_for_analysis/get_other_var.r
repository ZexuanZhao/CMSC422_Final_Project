args = commandArgs(trailingOnly=TRUE)

setwd(args[1])
wd<-getwd()

#read in taxonomy.txt to add variables
data<-read.table(args[2], sep="\t", as.is = TRUE)

for(i in 1:nrow(data)) {
	
	if (data$V5[i] == 'Mammalia' || data$V5[i] == 'Aves') {
		data$V8[i] <- 'endotherm'
		}
	else if (data$V7[i] == 'Plantae') {
		data$V8[i] <- 'photosynthesis'
		}
	else {
		data$V8[i] <- 'ectotherm'
		}
}		
		

for(i in 1:nrow(data)) {

	if (data$V5[i] == 'Amphibia' || data$V6[i] == 'Lycopodiophyta' || data$V6[i] == 'Pteridophyta') {
		data$V9[i] <- 'transitional'
		}
	else if (data$V4[i] == 'Chiroptera' || data$V5[i] == 'Aves' || data$V4[i] == 'Blattodea' || data$V4[i] == 'Coleoptera' || data$V4[i] == 'Diptera' || data$V4[i] == 'Hymenoptera' || data$V4[i] == 'Embioptera' || data$V4[i] == 'Ephemeroptera' || data$V4[i] == 'Lepidoptera' || data$V4[i] == 'Mecoptera' || data$V4[i] ==  'Megaloptera' || data$V4[i] == 'Neuroptera' || data$V4[i] == 'Odonata' || data$V4[i] == 'Orthoptera' || data$V4[i] == 'Thysanoptera' || data$V4[i] == 'Trichoptera') {
		data$V9[i] <- 'winged'
		}
	else if (data$V4[i] == 'Cetacea'|| data$V5[i] == 'Malacostraca' || data$V6[i] == 'Porifera' || data$V6[i] == 'Cnidaria' || data$V6[i] == 'Echinodermata' || data$V6[i] == 'Ctenophora' || data$V6[i] == 'Mollusca' || data$V5[i] == 'Actinopterygii' || data$V5[i] == 'Elasmobranchii' || data$V5[i] == 'Holocephali' || data$V5[i] == 'Myxini' || data$V5[i] ==  'Cephalaspidomorphi' || data$V5[i] == 'Sarcopterygii' || data$V6[i] == 'Anthocerotophyta' ||data$V6[i] == 'Bryophyta' || data$V6[i] == 'Marchantiophyta' || data$V5[i] == 'Polychaeta' || data$V5[i] == 'Neoophora' || data$V5[i] == 'Turbellaria') {
		data$V9[i] <- 'aquatic'
		}
    else if (data$V6[i] == 'Nematoda' || data$V5[i] == 'Trematoda' || data$V5[i] == 'Rhabditophora' || data$V5[i] == 'Cestoda') {
        data$V9[i] <- 'parasitic'
        }
    else {
		data$V9[i] <- 'terrestrial'
		}
		
}

write.table(data, sep="\t", file="taxonomy.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)