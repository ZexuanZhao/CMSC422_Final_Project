#to do projections
#load bioclim data for that time period

#load biomod.models.out
out_file<-load("/Volumes/HD2/mammal_niche_done/Apodemus peninsulae/NicheModel/NicheModel.Apodemus peninsulae.models.out")
out<-get(out_file)

#this needs to match biomod.models.out
list = c("bio_2", "bio_11", "bio_15", "bio_18", "bio_19")
lig_bioclim = stack(lig_bio_2, lig_bio_11, lig_bio_15, lig_bio_18, lig_bio_19)
names(lig_bioclim)<-c(list)

#must be in species directory so it can find path to modeling output
setwd("/Volumes/HD2/mammal_niche_done/Apodemus peninsulae")
lig_projection <- BIOMOD_Projection(
+   modeling.output = out,
+   new.env = lig_bioclim,
+   proj.name = "lig_testing",
+   selected.models = 'all',
+   binary.meth = NULL,
+   compress = 'xz',
+   build.clamping.mask = FALSE,
+   output.format = '.img')

lig <- raster("./NicheModel/proj_lig_testing/proj_lig_testing_NicheModel.img")
writeRaster(lig, file=paste(species,"_lig_testing",sep=""), format="ascii", overwrite=T) 
