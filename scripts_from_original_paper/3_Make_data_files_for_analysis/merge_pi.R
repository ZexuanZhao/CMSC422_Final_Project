setwd("")

ibe<-read.table("ibe.txt", sep="\t", stringsAsFactors = FALSE)
pi<-read.table("pi.txt", sep="\t")
tax<-read.table("taxonomy.txt", sep="\t")
var1<-read.table("geo_dist_var.txt", sep="\t")
var2<-read.table("additional_geo_dist_var.txt", sep="\t")

colnames(ibe) <- c("species","gene", "n", "pmtr", "pmtp", "dgr", "dgp", "der", "dep", "ger", "gep")
colnames(pi) <- c("species","gene", "pi")
colnames(tax) <- c("species","Genus", "Family", "Order", "Class", "Phylum", "Kingdom", "metabolism", "habit")
colnames(var1) <- c("species", "Post-flooding or irrigated croplands (or aquatic)", "Rainfed croplands", "Mosaic cropland (50-70%) / vegetation (grassland/shrubland/forest) (20-50%)", "Mosaic vegetation (grassland/shrubland/forest) (50-70%) / cropland (20-50%) ", "Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)", "Closed (>40%) broadleaved deciduous forest (>5m)", "Open (15-40%) broadleaved deciduous forest/woodland (>5m)", "Closed (>40%) needleleaved evergreen forest (>5m)", "Open (15-40%) needleleaved deciduous or evergreen forest (>5m)", "Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)", "Mosaic forest or shrubland (50-70%) / grassland (20-50%)", "Mosaic grassland (50-70%) / forest or shrubland (20-50%) ", "Closed to open (>15%) (broadleaved or needleleaved, evergreen or deciduous) shrubland (<5m)", "Closed to open (>15%) herbaceous vegetation (grassland, savannas or lichens/mosses)", "Sparse (<15%) vegetation", "Closed to open (>15%) broadleaved forest regularly flooded (semi-permanently or temporarily) - Fresh or brackish water", "Closed (>40%) broadleaved forest or shrubland permanently flooded - Saline or brackish water", "Closed to open (>15%) grassland or woody vegetation on regularly flooded or waterlogged soil - Fresh, brackish or saline water", "Artificial surfaces and associated areas (Urban areas >50%)", "Bare areas", "Water bodies", "Permanent snow and ice", "No data (burnt areas, clouds,…)", "area", "max_lat", "min_lat", "max_lon", "min_lon")
colnames(var2) <- c("species", "length_lat", "abs_min_lat", "abs_max_lat", "abs_mid_lat")

#head(ibe)
#head(pi)
#head(tax)
#head(var1)
#head(var2)
dim(ibe)
dim(pi)
dim(tax)
dim(var1)
dim(var2)

#merge all the data
temp1<-merge(ibe, pi, by=c("species","gene"), all=TRUE)
#head(temp1)
dim(temp1)
temp2<-merge(temp1, tax, by="species", all=TRUE)
#head(temp2)
dim(temp2)
temp3<-merge(temp2, var1, by="species", all=TRUE)
#head(temp3)
dim(temp3)
data<-merge(temp3, var2, by="species", all=TRUE)
dim(data)

data<-data[complete.cases(data[,c(2,3)]),]
#data<-unique(data)
#head(data)
dim(data)

#to change gene names to either mtDNA, nDNA, or cpDNA
for(i in 1:nrow(data)) {
    
    if (data$gene[i] == 'COI' || data$gene[i] == 'ND4' || data$gene[i] == 'CO1' || data$gene[i] == 'COII' || data$gene[i] == 'ND6' || data$gene[i] == 'cytochrome-oxidase-subunit-I' || data$gene[i] == 'coi' || data$gene[i] == 'cytb' || data$gene[i] == 'cytochrome-oxidase-subunit-1' || data$gene[i] == ' I' || data$gene[i] == 'COX1' || data$gene[i] == 'COX2' || data$gene[i] == 'cytochrome-oxidase-subunit-II' || data$gene[i] == 'NADH-dehydrogenase-subunit-1' || data$gene[i] == 'cytochrome-b' || data$gene[i] == 'cytochrome-c-oxidase-I' || data$gene[i] == 'cytochrome-oxidase-I' || data$gene[i] == 'coxII' || data$gene[i] == 'CoII' || data$gene[i] == 'CoI' || data$gene[i] == 'cytB' || data$gene[i] == 'ATP6' || data$gene[i] == 'ND2' || data$gene[i] == 'ND1' || data$gene[i] == 'Cytb' || data$gene[i] == 'Cox1cytochrome-oxidase-subunit-2' || data$gene[i] == 'cox1' || data$gene[i] == 'NADH-dehydrogenase-subunit-2' || data$gene[i] == 'CYTB' || data$gene[i] == 'NADH1' || data$gene[i] == 'CytB' || data$gene[i] == 'NAD1' || data$gene[i] == 'NAD4' || data$gene[i] == 'NAD5' || data$gene[i] == 'cytochrome-c-oxidase-subunit-I' || data$gene[i] == 'atp6' || data$gene[i] == 'atp8' || data$gene[i] == 'cox2' || data$gene[i] == 'cox3' || data$gene[i] == 'cob' || data$gene[i] == 'nad3' || data$gene[i] == 'nad4' || data$gene[i] == 'nad4L' || data$gene[i] == 'nad1' || data$gene[i] == 'nad6' || data$gene[i] == 'nad5' || data$gene[i] == 'NADH' || data$gene[i] == 'cytochrome-c-oxidase-subunit-1' || data$gene[i] == 'CO-I' || data$gene[i] == 'COIII' || data$gene[i] == 'nad7' || data$gene[i] == 'NADH6' || data$gene[i] == 'nad2' || data$gene[i] == 'cyt-b' || data$gene[i] == 'ATPSa' || data$gene[i] == 'NADH-dehydrogenase-subunit-3' || data$gene[i] == 'COX3' || data$gene[i] == 'ND3' || data$gene[i] == 'ND4L' || data$gene[i] == 'ND5' || data$gene[i] == 'CO3' || data$gene[i] == 'cytochrome-c-oxidase-subunit-II' || data$gene[i] == 'cytochrome-oxidase-subunit-III' || data$gene[i] == 'ATPB' || data$gene[i] == 'cox-1' || data$gene[i] == 'nd4' || data$gene[i] == 'Cyt-b' || data$gene[i] == 'CO2' || data$gene[i] == 'adenosine-triphosphate-synthase-6' || data$gene[i] == 'ATPase-subunit-6' || data$gene[i] == 'ATPase-subunit-8' || data$gene[i] == 'coii' || data$gene[i] == 'NADH-dehydrogenase-subunit-4' || data$gene[i] == 'COXIII' || data$gene[i] == 'NADH-dehydrogenase-subunit-II' || data$gene[i] == 'NADH2' || data$gene[i] == 'nad4L' || data$gene[i] == 'ATP8' || data$gene[i] == 'nd1' || data$gene[i] == 'nd2' || data$gene[i] == 'nd3' || data$gene[i] == 'nd5' || data$gene[i] == 'nd6' || data$gene[i] == 'NAD2' || data$gene[i] == 'NADH3') {
        data$gene[i] <- 'mtDNA'
    }
    
    else if (data$gene[i] == 'matK' || data$gene[i] == 'maturase-K' || data$gene[i] == 'matk' || data$gene[i] == 'rbcL' || data$gene[i] == 'ribulose-1,5-bisphosphate-carboxylase-oxygenase-large-subunit' || data$gene[i] == 'rpl32' || data$gene[i] == 'rpl2' || data$gene[i] == 'rpl22' || data$gene[i] == 'rpl36' || data$gene[i] == 'rpoc1' || data$gene[i] == 'rpoC1' || data$gene[i] == 'rps8' || data$gene[i] == 'rps16' || data$gene[i] == 'rps19' || data$gene[i] == 'rps4' || data$gene[i] == 'psbA' || data$gene[i] == 'psbZ' || data$gene[i] == 'PsbA' || data$gene[i] == 'petG' || data$gene[i] == 'psbK' || data$gene[i] == 'accD' || data$gene[i] == 'ndhF' || data$gene[i] == 'ycf9' || data$gene[i] == 'ribosomal-protein-S19' || data$gene[i] == 'hypothetical-protein' || data$gene[i] == 'rpoB' || data$gene[i] == 'psbN' || data$gene[i] == 'psbT' || data$gene[i] == 'ycf1' ||  data$gene[i] == 'cemA' || data$gene[i] == 'clpP' || data$gene[i] == 'petA' || data$gene[i] == 'petL' || data$gene[i] == 'petN' || data$gene[i] == 'psaJ' || data$gene[i] == 'psaM' || data$gene[i] == 'psbF' || data$gene[i] == 'psbE' || data$gene[i] == 'psbI' || data$gene[i] == 'psbK' || data$gene[i] == 'psbL' || data$gene[i] == 'psbJ' || data$gene[i] == 'psbM' || data$gene[i] == 'rpl20' || data$gene[i] == 'rpl33' || data$gene[i] == 'rpoC2' || data$gene[i] == 'rps18' || data$gene[i] == 'rps2' || data$gene[i] == 'ycf4' || data$gene[i] == 'AtpI' || data$gene[i] == 'atpH' || data$gene[i] == 'atpF' || data$gene[i] == 'atpA' || data$gene[i] == 'atpI' || data$gene[i] == 'ATP-synthase-CF0-B-subunit' || data$gene[i] == 'atpH' || data$gene[i] == 'atpF' || data$gene[i] == 'rpL16') {
        data$gene[i] <- 'cpDNA'
    }
    
    else {
        data$gene[i] <- 'nDNA'
    }
}

#head(data)

write.table(data, file="ibe_rf_data.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)


#NEXT STEP
# cat column header and all rf data tables