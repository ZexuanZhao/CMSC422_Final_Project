#results<-read.table("RESULTS.txt", sep="\t", stringsAsFactors = FALSE)
#write.table(unique(results$V2), file = "uniquegenes.txt", quote = FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)

setwd("")

results<-read.table("ibe.txt", sep="\t", stringsAsFactors = FALSE)
taxonomy<-read.table("taxonomy.txt", sep="\t")
geo_dist_var<-read.table("geo_dist_var.txt", sep="\t")
add_geo_dist_var<-read.table("additional_geo_dist_var.txt", sep="\t")
pi<-read.table("pi.txt", sep="\t")

dim(results)
head(results)
dim(taxonomy)
head(taxonomy)
dim(geo_dist_var)
head(geo_dist_var)
dim(add_geo_dist_var)
head(add_geo_dist_var)
dim(pi)
head(pi)



#merge all the data
data1<-merge(results, pi, by=c("V1","V2", "V3"), all.x=TRUE, all.y=FALSE)
data1<-unique(data1)
dim(data1)
head(data1)

data2<-merge(data1, taxonomy, by.x="V1", by.y="V1", all.x=TRUE, all.y=FALSE)
dim(data2)
head(data2)
data2<-unique(data2)

data3<-merge(data2, geo_dist_var, by.x="V1", by.y="V1", all.x=TRUE, all.y=FALSE)
dim(data3)
head(data3)
data3<-unique(data3)

data4<-merge(data3, add_geo_dist_var, by.x="V1", by.y="V1", all.x=TRUE, all.y=FALSE)
dim(data4)
head(data4)
data4<-unique(data4)

#import(mmrr)
names(MMRR)<-c("Species", "gene", "n")
names(data4)<-c("Species", "gene", "n")
MMRR$V1<-as.character(MMRR$V1)
MMRR$V2.x<-as.character(MMRR$V2.x)
data5<-merge(data4, MMRR, by=c("Species", "gene", "n"), all.x=TRUE, all.y=FALSE)
data5<-unique(data5)


names(data5)<-c("Species", "Gene", "n", "pmtr", "pmtp", "dgr", "dgp", "der", "dep", "ger", "gep", "bp", "pi", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom", "Metabolism", "Habit", "Post-flooding or irrigated croplands (or aquatic)","Rainfed croplands","Mosaic cropland (50-70%) / vegetation (grassland/shrubland/forest) (20-50%)","Mosaic vegetation (grassland/shrubland/forest) (50-70%) / cropland (20-50%)","Closed to open (>15%) broadleaved evergreen or semi-deciduous forest (>5m)","Closed (>40%) broadleaved deciduous forest (>5m)","Open (15-40%) broadleaved deciduous forest/woodland (>5m)","Closed (>40%) needleleaved evergreen forest (>5m)","Open (15-40%) needleleaved deciduous or evergreen forest (>5m)","Closed to open (>15%) mixed broadleaved and needleleaved forest (>5m)","Mosaic forest or shrubland (50-70%) / grassland (20-50%)","Mosaic grassland (50-70%) / forest or shrubland (20-50%)","Closed to open (>15%) (broadleaved or needleleaved, evergreen or deciduous) shrubland (<5m)","Closed to open (>15%) herbaceous vegetation (grassland, savannas or lichens/mosses)","Sparse (<15%) vegetation","Closed to open (>15%) broadleaved forest regularly flooded (semi-permanently or temporarily) - Fresh or brackish water","Closed (>40%) broadleaved forest or shrubland permanently flooded - Saline or brackish water","Closed to open (>15%) grassland or woody vegetation on regularly flooded or waterlogged soil - Fresh, brackish or saline water","Artificial surfaces and associated areas (Urban areas >50%)","Bare areas","Water bodies","Permanent snow and ice","No data (burnt areas, clouds,.)", "area", "max_lat", "min_lat", "max_lon", "min_lon", "length", "abs_min_lat", "abs_max_lat", "abs_mid_lat", "r","c","t","p","f","fp","cgeo","tgeo","pgeo","cenv","tenv","penv")


write.table(data5, file="rf_data_table.txt", quote = FALSE, sep = "\t", row.names=FALSE)

#to change gene names to either mtDNA, nDNA, or cpDNA
for(i in 1:nrow(data5)) {
  
  if (data5$Gene[i] == 'COI' || data5$Gene[i] == 'ND4' || data5$Gene[i] == 'CO1' || data5$Gene[i] == 'COII' || data5$Gene[i] == 'ND6' || data5$Gene[i] == 'cytochrome-oxidase-subunit-I' || data5$Gene[i] == 'coi' || data5$Gene[i] == 'cytb' || data5$Gene[i] == 'cytochrome-oxidase-subunit-1' || data5$Gene[i] == ' I' || data5$Gene[i] == 'COX1' || data5$Gene[i] == 'COX2' || data5$Gene[i] == 'cytochrome-oxidase-subunit-II' || data5$Gene[i] == 'NADH-dehydrogenase-subunit-1' || data5$Gene[i] == 'cytochrome-b' || data5$Gene[i] == 'cytochrome-c-oxidase-I' || data5$Gene[i] == 'cytochrome-oxidase-I' || data5$Gene[i] == 'coxII' || data5$Gene[i] == 'CoII' || data5$Gene[i] == 'CoI' || data5$Gene[i] == 'cytB' || data5$Gene[i] == 'ATP6' || data5$Gene[i] == 'ND2' || data5$Gene[i] == 'ND1' || data5$Gene[i] == 'Cytb' || data5$Gene[i] == 'Cox1cytochrome-oxidase-subunit-2' || data5$Gene[i] == 'cox1' || data5$Gene[i] == 'NADH-dehydrogenase-subunit-2' || data5$Gene[i] == 'CYTB' || data5$Gene[i] == 'NADH1' || data5$Gene[i] == 'CytB' || data5$Gene[i] == 'NAD1' || data5$Gene[i] == 'NAD4' || data5$Gene[i] == 'NAD5' || data5$Gene[i] == 'cytochrome-c-oxidase-subunit-I' || data5$Gene[i] == 'atp6' || data5$Gene[i] == 'atp8' || data5$Gene[i] == 'cox2' || data5$Gene[i] == 'cox3' || data5$Gene[i] == 'cob' || data5$Gene[i] == 'nad3' || data5$Gene[i] == 'nad4' || data5$Gene[i] == 'nad4L' || data5$Gene[i] == 'nad1' || data5$Gene[i] == 'nad6' || data5$Gene[i] == 'nad5' || data5$Gene[i] == 'NADH' || data5$Gene[i] == 'cytochrome-c-oxidase-subunit-1' || data5$Gene[i] == 'CO-I' || data5$Gene[i] == 'COIII' || data5$Gene[i] == 'nad7' || data5$Gene[i] == 'NADH6' || data5$Gene[i] == 'nad2' || data5$Gene[i] == 'cyt-b' || data5$Gene[i] == 'ATPSa' || data5$Gene[i] == 'NADH-dehydrogenase-subunit-3' || data5$Gene[i] == 'COX3' || data5$Gene[i] == 'ND3' || data5$Gene[i] == 'ND4L' || data5$Gene[i] == 'ND5' || data5$Gene[i] == 'CO3' || data5$Gene[i] == 'cytochrome-c-oxidase-subunit-II' || data5$Gene[i] == 'cytochrome-oxidase-subunit-III' || data5$Gene[i] == 'ATPB' || data5$Gene[i] == 'cox-1' || data5$Gene[i] == 'nd4' || data5$Gene[i] == 'Cyt-b' || data5$Gene[i] == 'CO2' || data5$Gene[i] == 'adenosine-triphosphate-synthase-6' || data5$Gene[i] == 'ATPase-subunit-6' || data5$Gene[i] == 'ATPase-subunit-8' || data5$Gene[i] == 'coii' || data5$Gene[i] == 'NADH-dehydrogenase-subunit-4' || data5$Gene[i] == 'COXIII' || data5$Gene[i] == 'NADH-dehydrogenase-subunit-II' || data5$Gene[i] == 'NADH2' || data5$Gene[i] == 'nad4L' || data5$Gene[i] == 'ATP8' || data5$Gene[i] == 'nd1' || data5$Gene[i] == 'nd2' || data5$Gene[i] == 'nd3' || data5$Gene[i] == 'nd5' || data5$Gene[i] == 'nd6' || data5$Gene[i] == 'NAD2' || data5$Gene[i] == 'NADH3') {
    data5$Gene[i] <- 'mtDNA'
  }
  
  else if (data5$Gene[i] == 'matK' || data5$Gene[i] == 'maturase-K' || data5$Gene[i] == 'matk' || data5$Gene[i] == 'rbcL' || data5$Gene[i] == 'ribulose-1,5-bisphosphate-carboxylase-oxygenase-large-subunit' || data5$Gene[i] == 'rpl32' || data5$Gene[i] == 'rpl2' || data5$Gene[i] == 'rpl22' || data5$Gene[i] == 'rpl36' || data5$Gene[i] == 'rpoc1' || data5$Gene[i] == 'rpoC1' || data5$Gene[i] == 'rps8' || data5$Gene[i] == 'rps16' || data5$Gene[i] == 'rps19' || data5$Gene[i] == 'rps4' || data5$Gene[i] == 'psbA' || data5$Gene[i] == 'psbZ' || data5$Gene[i] == 'PsbA' || data5$Gene[i] == 'petG' || data5$Gene[i] == 'psbK' || data5$Gene[i] == 'accD' || data5$Gene[i] == 'ndhF' || data5$Gene[i] == 'ycf9' || data5$Gene[i] == 'ribosomal-protein-S19' || data5$Gene[i] == 'hypothetical-protein' || data5$Gene[i] == 'rpoB' || data5$Gene[i] == 'psbN' || data5$Gene[i] == 'psbT' || data5$Gene[i] == 'ycf1' ||  data5$Gene[i] == 'cemA' || data5$Gene[i] == 'clpP' || data5$Gene[i] == 'petA' || data5$Gene[i] == 'petL' || data5$Gene[i] == 'petN' || data5$Gene[i] == 'psaJ' || data5$Gene[i] == 'psaM' || data5$Gene[i] == 'psbF' || data5$Gene[i] == 'psbE' || data5$Gene[i] == 'psbI' || data5$Gene[i] == 'psbK' || data5$Gene[i] == 'psbL' || data5$Gene[i] == 'psbJ' || data5$Gene[i] == 'psbM' || data5$Gene[i] == 'rpl20' || data5$Gene[i] == 'rpl33' || data5$Gene[i] == 'rpoC2' || data5$Gene[i] == 'rps18' || data5$Gene[i] == 'rps2' || data5$Gene[i] == 'ycf4' || data5$Gene[i] == 'AtpI' || data5$Gene[i] == 'atpH' || data5$Gene[i] == 'atpF' || data5$Gene[i] == 'atpA' || data5$Gene[i] == 'atpI' || data5$Gene[i] == 'ATP-synthase-CF0-B-subunit' || data5$Gene[i] == 'atpH' || data5$Gene[i] == 'atpF' || data5$Gene[i] == 'rpL16') {
    data5$Gene[i] <- 'cpDNA'
  }
  
  else {
    data5$Gene[i] <- 'nDNA'
  }
}



#NEXT STEP
# cat /Users/Tara/Desktop/Tara2016/DATA/column_headings.txt data_table_ibd_rf_no_headings.txt > data_table_ibd_rf.txt