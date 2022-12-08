library(raster)
library(biomod2)
library(maps)
library(mapdata)
library(rgdal)

args = commandArgs(trailingOnly=TRUE)

setwd(args[1])
wd<-getwd()

#name of study species
#if from folder
species <- basename(wd)
#if from files
#species = args[3]

#import bioclim data
# w for worldwide
bio1w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_1.bil")
bio2w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_2.bil")
bio3w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_3.bil")
bio4w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_4.bil")
bio5w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_5.bil")
bio6w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_6.bil")
bio7w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_7.bil")
bio8w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_8.bil")
bio9w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_9.bil")
bio10w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_10.bil")
bio11w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_11.bil")
bio12w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_12.bil")
bio13w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_13.bil")
bio14w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_14.bil")
bio15w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_15.bil")
bio16w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_16.bil")
bio17w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_17.bil")
bio18w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_18.bil")
bio19w = raster("/Volumes/HD2/Tara2016/BIOCLIM/bio_30seconds/bio_19.bil")

#read gps points file
gps<-read.table(args[2])

#get min and max lat and lon values - add 10 degrees to each for mapping/projection
#assumes points represent full range of species
#points should be in order LON, LAT for R analyses
max_lat <- max(gps$V4)
#max_lat
high_lat <- max_lat + 10
#high_lat
min_lat <- min(gps$V4)
#min_lat
low_lat <-  min_lat - 10
#low_lat
max_lon <- max(gps$V5)
#max_lon
high_lon <- max_lon + 10
#high_lon
min_lon <- min(gps$V5)
#min_lon
low_lon <-	min_lon - 10
#low_lon

#check points
#wmap <- getMap(resolution = "low")
#plot(wmap, xlim = c(low_lon, high_lon), ylim = c(low_lat, high_lat), asp=1)
#points(gps$V5, gps$V4, col="red", pch=20, cex=0.5)

#clip biolayers
range = extent(c(low_lon, high_lon, low_lat, high_lat))
bio_1 = crop(bio1w, range)
bio_2 = crop(bio2w, range)
bio_3 = crop(bio3w, range)
bio_4 = crop(bio4w, range)
bio_5 = crop(bio5w, range)
bio_6 = crop(bio6w, range)
bio_7 = crop(bio7w, range)
bio_8 = crop(bio8w, range)
bio_9 = crop(bio9w, range)
bio_10 = crop(bio10w, range)
bio_11 = crop(bio11w, range)
bio_12 = crop(bio12w, range)
bio_13 = crop(bio13w, range)
bio_14 = crop(bio14w, range)
bio_15 = crop(bio15w, range)
bio_16 = crop(bio16w, range)
bio_17 = crop(bio17w, range)
bio_18 = crop(bio18w, range)
bio_19 = crop(bio19w, range)

bios = stack(bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19)
list = names(bios)

#remove all unused worldwide layers to free the memory
rm(bio1w, bio2w, bio3w, bio4w, bio5w, bio6w, bio7w, bio8w, bio9w, bio10w, bio11w, bio12w, bio13w, bio14w, bio15w, bio16w, bio17w, bio18w, bio19w)

#get correlations among variables and remove the high ones
#this step is SLOW
s <- layerStats(bios, stat = "pearson", na.rm=TRUE)

#function to convert cor matrix into useable list
flattenSquareMatrix <- function(m) {
   if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
   if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
   ut <- upper.tri(m)
   data.frame(i = rownames(m)[row(m)[ut]],
              j = rownames(m)[col(m)[ut]],
              cor=t(m)[ut],
              p=m[ut])
 }

#determine which variables to drop
f <-flattenSquareMatrix(s$'pearson correlation coefficient')
g <- subset (f, f$cor > 0.7 | f$cor < -0.7)
remove <- sort(unique(g$i))
#remove

#make new list of bioclim variables for modeling
new_list_q <- list[! list %in% remove]
new_list <- noquote(paste(new_list_q, collapse=", "))
#new_list

#bioclim layers file
bioclim = eval(parse(text = paste("stack(",new_list,")")))

#format gps points for niche modeling
gpspoints<- gps[c(5,4)]
gpspoints<- gpspoints[!duplicated(gpspoints),]
n<-nrow(gpspoints)

#make presence data information
p<-matrix(1, ncol = 1, nrow = n)


#NICHE MODEL
BiomodData <- BIOMOD_FormatingData(resp.var = p,
                                     expl.var = bioclim,
                                     resp.xy = yx,
                                     resp.name = "NicheModel",
                                     PA.nb.rep=1,
                                     PA.nb.absences=10000,
                                     PA.strategy="random")

#define parameters for MaxEnt model
BiomodOption <- BIOMOD_ModelingOptions(
  MAXENT.Phillips = list( path_to_maxent.jar = "PATH",
                 maximumiterations = 1000,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = TRUE,
                 threshold = TRUE,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1, 
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 defaultprevalence = 0.5 ))

#run the model with pseudo absences 
BiomodModelOut <- BIOMOD_Modeling(
  BiomodData,
  models = c('MAXENT.Phillips'),
  models.options = BiomodOption,
  NbRunEval=1,
  DataSplit=70, #30% of the samples are set aside for model evaluation
  VarImport=1, #number of iterations for evaluation variable importance
  models.eval.meth = c('ROC', 'TSS'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  modeling.id = species)
  
  
#get model evalutaion values
evals <- get_evaluations(BiomodModelOut) 
tss <- evals[2,1,1,2,1]
roc <- evals[1,1,1,2,1]

#get bioclim variable importance values
variables<-get_variables_importance(BiomodModelOut)
v<-variables[,,2,]
v<-sort(v, decreasing=TRUE)

#outputdata we might want later
write.table(data.frame(n, tss, roc), file = paste(species,"_niche_values.txt", sep=""), sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
write.table(v, file = paste(species,"_bioclim_values.txt", sep=""), col.names=FALSE, quote=FALSE, sep = "\t")

#project model on geographic space
projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = bioclim,
  proj.name = "current",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')

#export the projection to a raster ascii file for later use
current <- raster("./NicheModel/proj_current/proj_current_NicheModel.img")
writeRaster(current, file=paste(species,"_current",sep=""), format="ascii", overwrite=T)

#plot points and projection on map and send to file
pdf(paste(species, "_map.pdf", sep=""))
plot(current, xlab="longitude", ylab="latitude")
points(gps$V5, gps$V4, col="navy", pch=20, cex=0.5)
map(xlim = c(low_lon, high_lon), ylim = c(low_lat, high_lat), add=TRUE)
dev.off()


#######################################
#project model into the past and future


### Last Interglacial
#import bioclim data
bio1wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_1.bil")
bio2wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_2.bil")
bio3wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_3.bil")
bio4wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_4.bil")
bio5wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_5.bil")
bio6wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_6.bil")
bio7wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_7.bil")
bio8wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_8.bil")
bio9wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_9.bil")
bio10wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_10.bil")
bio11wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_11.bil")
bio12wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_12.bil")
bio13wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_13.bil")
bio14wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_14.bil")
bio15wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_15.bil")
bio16wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_16.bil")
bio17wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_17.bil")
bio18wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_18.bil")
bio19wlig = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lig_bio_30seconds/lig_30s_bio_19.bil")

#clip biolayers
lig_bio_1 = crop(bio1wlig, range)
lig_bio_2 = crop(bio2wlig, range)
lig_bio_3 = crop(bio3wlig, range)
lig_bio_4 = crop(bio4wlig, range)
lig_bio_5 = crop(bio5wlig, range)
lig_bio_6 = crop(bio6wlig, range)
lig_bio_7 = crop(bio7wlig, range)
lig_bio_8 = crop(bio8wlig, range)
lig_bio_9 = crop(bio9wlig, range)
lig_bio_10 = crop(bio10wlig, range)
lig_bio_11 = crop(bio11wlig, range)
lig_bio_12 = crop(bio12wlig, range)
lig_bio_13 = crop(bio13wlig, range)
lig_bio_14 = crop(bio14wlig, range)
lig_bio_15 = crop(bio15wlig, range)
lig_bio_16 = crop(bio16wlig, range)
lig_bio_17 = crop(bio17wlig, range)
lig_bio_18 = crop(bio18wlig, range)
lig_bio_19 = crop(bio19wlig, range)

rm(bio1wlig, bio2wlig, bio3wlig, bio4wlig, bio5wlig, bio6wlig, bio7wlig, bio8wlig, bio9wlig, bio10wlig, bio11wlig, bio12wlig, bio13wlig, bio14wlig, bio15wlig, bio16wlig, bio17wlig, bio18wlig, bio19wlig)

#get bioclim variables for model
lig_list<-gsub("bio", "lig_bio", new_list)

#stack bioclim layers file
lig_bioclim = eval(parse(text = paste("stack(",lig_list,")")))
#lig_bioclim
names(lig_bioclim)<-c(new_list_q)
#lig_bioclim

#project model
lig_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = lig_bioclim,
  proj.name = "lig",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
lig <- raster("./NicheModel/proj_lig/proj_lig_NicheModel.img")
writeRaster(lig, file=paste(species,"_lig",sep=""), format="ascii", overwrite=T) 

rm(lig_bio_1, lig_bio_2, lig_bio_3, lig_bio_4, lig_bio_5, lig_bio_6, lig_bio_7, lig_bio_8, lig_bio_9, lig_bio_10, lig_bio_11, lig_bio_12, lig_bio_13, lig_bio_14, lig_bio_15, lig_bio_16, lig_bio_17, lig_bio_18, lig_bio_19, lig_list, lig_bioclim, lig_projection, lig)


### Last Glacial Maximum
#import bioclim data
bio1wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi1.tif")
bio2wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi2.tif")
bio3wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi3.tif")
bio4wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi4.tif")
bio5wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi5.tif")
bio6wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi6.tif")
bio7wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi7.tif")
bio8wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi8.tif")
bio9wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi9.tif")
bio10wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi10.tif")
bio11wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi11.tif")
bio12wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi12.tif")
bio13wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi13.tif")
bio14wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi14.tif")
bio15wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi15.tif")
bio16wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi16.tif")
bio17wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi17.tif")
bio18wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi18.tif")
bio19wlgm = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_lgm_bio_2.5minute/mrlgmbi19.tif")

#clip biolayers
lgm_bio_1 = crop(bio1wlgm, range)
lgm_bio_2 = crop(bio2wlgm, range)
lgm_bio_3 = crop(bio3wlgm, range)
lgm_bio_4 = crop(bio4wlgm, range)
lgm_bio_5 = crop(bio5wlgm, range)
lgm_bio_6 = crop(bio6wlgm, range)
lgm_bio_7 = crop(bio7wlgm, range)
lgm_bio_8 = crop(bio8wlgm, range)
lgm_bio_9 = crop(bio9wlgm, range)
lgm_bio_10 = crop(bio10wlgm, range)
lgm_bio_11 = crop(bio11wlgm, range)
lgm_bio_12 = crop(bio12wlgm, range)
lgm_bio_13 = crop(bio13wlgm, range)
lgm_bio_14 = crop(bio14wlgm, range)
lgm_bio_15 = crop(bio15wlgm, range)
lgm_bio_16 = crop(bio16wlgm, range)
lgm_bio_17 = crop(bio17wlgm, range)
lgm_bio_18 = crop(bio18wlgm, range)
lgm_bio_19 = crop(bio19wlgm, range)

rm(bio1wlgm, bio2wlgm, bio3wlgm, bio4wlgm, bio5wlgm, bio6wlgm, bio7wlgm, bio8wlgm, bio9wlgm, bio10wlgm, bio11wlgm, bio12wlgm, bio13wlgm, bio14wlgm, bio15wlgm, bio16wlgm, bio17wlgm, bio18wlgm, bio19wlgm)

#get bioclim variables for model
lgm_list<-gsub("bio", "lgm_bio", new_list)

#stack bioclim layers file
lgm_bioclim = eval(parse(text = paste("stack(",lgm_list,")")))
#lgm_bioclim
names(lgm_bioclim)<-c(new_list_q)
#lgm_bioclim

#project model
lgm_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = lgm_bioclim,
  proj.name = "lgm",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
lgm <- raster("./NicheModel/proj_lgm/proj_lgm_NicheModel.img")
writeRaster(lgm, file=paste(species,"_lgm",sep=""), format="ascii",overwrite=T) 

rm(lgm_bio_1, lgm_bio_2, lgm_bio_3, lgm_bio_4, lgm_bio_5, lgm_bio_6, lgm_bio_7, lgm_bio_8, lgm_bio_9, lgm_bio_10, lgm_bio_11, lgm_bio_12, lgm_bio_13, lgm_bio_14, lgm_bio_15, lgm_bio_16, lgm_bio_17, lgm_bio_18, lgm_bio_19, lgm_list, lgm_bioclim, lgm_projection, lgm)


### Mid - Holocene
#import bioclim data
bio1wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi1.tif")
bio2wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi2.tif")
bio3wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi3.tif")
bio4wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi4.tif")
bio5wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi5.tif")
bio6wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi6.tif")
bio7wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi7.tif")
bio8wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi8.tif")
bio9wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi9.tif")
bio10wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi10.tif")
bio11wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi11.tif")
bio12wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi12.tif")
bio13wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi13.tif")
bio14wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi14.tif")
bio15wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi15.tif")
bio16wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi16.tif")
bio17wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi17.tif")
bio18wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi18.tif")
bio19wmh = raster("/Volumes/HD2/Tara2016/BIOCLIM/past_mh_bio_2.5minute/mrmidbi19.tif")

#clip biolayers
mh_bio_1 = crop(bio1wmh, range)
mh_bio_2 = crop(bio2wmh, range)
mh_bio_3 = crop(bio3wmh, range)
mh_bio_4 = crop(bio4wmh, range)
mh_bio_5 = crop(bio5wmh, range)
mh_bio_6 = crop(bio6wmh, range)
mh_bio_7 = crop(bio7wmh, range)
mh_bio_8 = crop(bio8wmh, range)
mh_bio_9 = crop(bio9wmh, range)
mh_bio_10 = crop(bio10wmh, range)
mh_bio_11 = crop(bio11wmh, range)
mh_bio_12 = crop(bio12wmh, range)
mh_bio_13 = crop(bio13wmh, range)
mh_bio_14 = crop(bio14wmh, range)
mh_bio_15 = crop(bio15wmh, range)
mh_bio_16 = crop(bio16wmh, range)
mh_bio_17 = crop(bio17wmh, range)
mh_bio_18 = crop(bio18wmh, range)
mh_bio_19 = crop(bio19wmh, range)

rm(bio1wmh, bio2wmh, bio3wmh, bio4wmh, bio5wmh, bio6wmh, bio7wmh, bio8wmh, bio9wmh, bio10wmh, bio11wmh, bio12wmh, bio13wmh, bio14wmh, bio15wmh, bio16wmh, bio17wmh, bio18wmh, bio19wmh)

#get bioclim variables for model
mh_list<-gsub("bio", "mh_bio", new_list)

#stack bioclim layers file
mh_bioclim = eval(parse(text = paste("stack(",mh_list,")")))
#mh_bioclim
names(mh_bioclim)<-c(new_list_q)
#mh_bioclim

#project model
mh_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = mh_bioclim,
  proj.name = "mh",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
mh <- raster("./NicheModel/proj_mh/proj_mh_NicheModel.img")
writeRaster(mh, file=paste(species,"_mh",sep=""), format="ascii",overwrite=T)

rm(mh_bio_1, mh_bio_2, mh_bio_3, mh_bio_4, mh_bio_5, mh_bio_6, mh_bio_7, mh_bio_8, mh_bio_9, mh_bio_10, mh_bio_11, mh_bio_12, mh_bio_13, mh_bio_14, mh_bio_15, mh_bio_16, mh_bio_17, mh_bio_18, mh_bio_19, mh_list, mh_bioclim, mh_projection, mh)


### Future 2.6 global warming increase model at 50 years 
#import bioclim data
bio1wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi501.tif")
bio2wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi502.tif")
bio3wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi503.tif")
bio4wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi504.tif")
bio5wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi505.tif")
bio6wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi506.tif")
bio7wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi507.tif")
bio8wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi508.tif")
bio9wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi509.tif")
bio10wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5010.tif")
bio11wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5011.tif")
bio12wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5012.tif")
bio13wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5013.tif")
bio14wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5014.tif")
bio15wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5015.tif")
bio16wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5016.tif")
bio17wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5017.tif")
bio18wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5018.tif")
bio19wf2650 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2650_bio_30seconds/mr26bi5019.tif")

#clip biolayers
f2650_bio_1 = crop(bio1wf2650, range)
f2650_bio_2 = crop(bio2wf2650, range)
f2650_bio_3 = crop(bio3wf2650, range)
f2650_bio_4 = crop(bio4wf2650, range)
f2650_bio_5 = crop(bio5wf2650, range)
f2650_bio_6 = crop(bio6wf2650, range)
f2650_bio_7 = crop(bio7wf2650, range)
f2650_bio_8 = crop(bio8wf2650, range)
f2650_bio_9 = crop(bio9wf2650, range)
f2650_bio_10 = crop(bio10wf2650, range)
f2650_bio_11 = crop(bio11wf2650, range)
f2650_bio_12 = crop(bio12wf2650, range)
f2650_bio_13 = crop(bio13wf2650, range)
f2650_bio_14 = crop(bio14wf2650, range)
f2650_bio_15 = crop(bio15wf2650, range)
f2650_bio_16 = crop(bio16wf2650, range)
f2650_bio_17 = crop(bio17wf2650, range)
f2650_bio_18 = crop(bio18wf2650, range)
f2650_bio_19 = crop(bio19wf2650, range)

rm(bio1wf2650, bio2wf2650, bio3wf2650, bio4wf2650, bio5wf2650, bio6wf2650, bio7wf2650, bio8wf2650, bio9wf2650, bio10wf2650, bio11wf2650, bio12wf2650, bio13wf2650, bio14wf2650, bio15wf2650, bio16wf2650, bio17wf2650, bio18wf2650, bio19wf2650)

#get bioclim variables for model
f2650_list<-gsub("bio", "f2650_bio", new_list)

#stack bioclim layers file
f2650_bioclim = eval(parse(text = paste("stack(",f2650_list,")")))
#f2650_bioclim
names(f2650_bioclim)<-c(new_list_q)
#f2650_bioclim

#project model
f2650_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f2650_bioclim,
  proj.name = "f2650",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f2650 <- raster("./NicheModel/proj_f2650/proj_f2650_NicheModel.img")
writeRaster(f2650, file=paste(species,"_f2650",sep=""), format="ascii",overwrite=T)

rm(f2650_bio_1, f2650_bio_2, f2650_bio_3, f2650_bio_4, f2650_bio_5, f2650_bio_6, f2650_bio_7, f2650_bio_8, f2650_bio_9, f2650_bio_10, f2650_bio_11, f2650_bio_12, f2650_bio_13, f2650_bio_14, f2650_bio_15, f2650_bio_16, f2650_bio_17, f2650_bio_18, f2650_bio_19, f2650_list, f2650_bioclim, f2650_projection, f2650)


### Future 2.6 global warming increase model at 70 years 
#import bioclim data
bio1wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi701.tif")
bio2wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi702.tif")
bio3wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi703.tif")
bio4wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi704.tif")
bio5wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi705.tif")
bio6wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi706.tif")
bio7wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi707.tif")
bio8wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi708.tif")
bio9wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi709.tif")
bio10wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7010.tif")
bio11wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7011.tif")
bio12wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7012.tif")
bio13wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7013.tif")
bio14wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7014.tif")
bio15wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7015.tif")
bio16wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7016.tif")
bio17wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7017.tif")
bio18wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7018.tif")
bio19wf2670 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f2670_bio_30seconds/mr26bi7019.tif")

#clip biolayers
f2670_bio_1 = crop(bio1wf2670, range)
f2670_bio_2 = crop(bio2wf2670, range)
f2670_bio_3 = crop(bio3wf2670, range)
f2670_bio_4 = crop(bio4wf2670, range)
f2670_bio_5 = crop(bio5wf2670, range)
f2670_bio_6 = crop(bio6wf2670, range)
f2670_bio_7 = crop(bio7wf2670, range)
f2670_bio_8 = crop(bio8wf2670, range)
f2670_bio_9 = crop(bio9wf2670, range)
f2670_bio_10 = crop(bio10wf2670, range)
f2670_bio_11 = crop(bio11wf2670, range)
f2670_bio_12 = crop(bio12wf2670, range)
f2670_bio_13 = crop(bio13wf2670, range)
f2670_bio_14 = crop(bio14wf2670, range)
f2670_bio_15 = crop(bio15wf2670, range)
f2670_bio_16 = crop(bio16wf2670, range)
f2670_bio_17 = crop(bio17wf2670, range)
f2670_bio_18 = crop(bio18wf2670, range)
f2670_bio_19 = crop(bio19wf2670, range)

rm(bio1wf2670, bio2wf2670, bio3wf2670, bio4wf2670, bio5wf2670, bio6wf2670, bio7wf2670, bio8wf2670, bio9wf2670, bio10wf2670, bio11wf2670, bio12wf2670, bio13wf2670, bio14wf2670, bio15wf2670, bio16wf2670, bio17wf2670, bio18wf2670, bio19wf2670)

#get bioclim variables for model
f2670_list<-gsub("bio", "f2670_bio", new_list)

#stack bioclim layers file
f2670_bioclim = eval(parse(text = paste("stack(",f2670_list,")")))
#f2670_bioclim
names(f2670_bioclim)<-c(new_list_q)
#f2670_bioclim

#project model
f2670_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f2670_bioclim,
  proj.name = "f2670",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f2670 <- raster("./NicheModel/proj_f2670/proj_f2670_NicheModel.img")
writeRaster(f2670, file=paste(species,"_f2670",sep=""), format="ascii",overwrite=T)

rm(f2670_bio_1, f2670_bio_2, f2670_bio_3, f2670_bio_4, f2670_bio_5, f2670_bio_6, f2670_bio_7, f2670_bio_8, f2670_bio_9, f2670_bio_10, f2670_bio_11, f2670_bio_12, f2670_bio_13, f2670_bio_14, f2670_bio_15, f2670_bio_16, f2670_bio_17, f2670_bio_18, f2670_bio_19, f2670_list, f2670_bioclim, f2670_projection, f2670)


### Future 4.5 global warming increase model at 50 years
#import bioclim data
bio1wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi501.tif")
bio2wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi502.tif")
bio3wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi503.tif")
bio4wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi504.tif")
bio5wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi505.tif")
bio6wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi506.tif")
bio7wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi507.tif")
bio8wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi508.tif")
bio9wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi509.tif")
bio10wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5010.tif")
bio11wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5011.tif")
bio12wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5012.tif")
bio13wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5013.tif")
bio14wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5014.tif")
bio15wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5015.tif")
bio16wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5016.tif")
bio17wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5017.tif")
bio18wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5018.tif")
bio19wf4550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4550_bio_30seconds/mr45bi5019.tif")

#clip biolayers
f4550_bio_1 = crop(bio1wf4550, range)
f4550_bio_2 = crop(bio2wf4550, range)
f4550_bio_3 = crop(bio3wf4550, range)
f4550_bio_4 = crop(bio4wf4550, range)
f4550_bio_5 = crop(bio5wf4550, range)
f4550_bio_6 = crop(bio6wf4550, range)
f4550_bio_7 = crop(bio7wf4550, range)
f4550_bio_8 = crop(bio8wf4550, range)
f4550_bio_9 = crop(bio9wf4550, range)
f4550_bio_10 = crop(bio10wf4550, range)
f4550_bio_11 = crop(bio11wf4550, range)
f4550_bio_12 = crop(bio12wf4550, range)
f4550_bio_13 = crop(bio13wf4550, range)
f4550_bio_14 = crop(bio14wf4550, range)
f4550_bio_15 = crop(bio15wf4550, range)
f4550_bio_16 = crop(bio16wf4550, range)
f4550_bio_17 = crop(bio17wf4550, range)
f4550_bio_18 = crop(bio18wf4550, range)
f4550_bio_19 = crop(bio19wf4550, range)

rm(bio1wf4550, bio2wf4550, bio3wf4550, bio4wf4550, bio5wf4550, bio6wf4550, bio7wf4550, bio8wf4550, bio9wf4550, bio10wf4550, bio11wf4550, bio12wf4550, bio13wf4550, bio14wf4550, bio15wf4550, bio16wf4550, bio17wf4550, bio18wf4550, bio19wf4550)

#get bioclim variables for model
f4550_list<-gsub("bio", "f4550_bio", new_list)

#stack bioclim layers file
f4550_bioclim = eval(parse(text = paste("stack(",f4550_list,")")))
#f4550_bioclim
names(f4550_bioclim)<-c(new_list_q)
#f4550_bioclim

#project model
f4550_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f4550_bioclim,
  proj.name = "f4550",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f4550 <- raster("./NicheModel/proj_f4550/proj_f4550_NicheModel.img")
writeRaster(f4550, file=paste(species,"_f4550",sep=""), format="ascii",overwrite=T)

rm(f4550_bio_1, f4550_bio_2, f4550_bio_3, f4550_bio_4, f4550_bio_5, f4550_bio_6, f4550_bio_7, f4550_bio_8, f4550_bio_9, f4550_bio_10, f4550_bio_11, f4550_bio_12, f4550_bio_13, f4550_bio_14, f4550_bio_15, f4550_bio_16, f4550_bio_17, f4550_bio_18, f4550_bio_19, f4550_list, f4550_bioclim, f4550_projection, f4550)


### Future 4.5 global warming increase model at 70 years
#import bioclim data
bio1wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi701.tif")
bio2wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi702.tif")
bio3wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi703.tif")
bio4wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi704.tif")
bio5wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi705.tif")
bio6wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi706.tif")
bio7wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi707.tif")
bio8wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi708.tif")
bio9wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi709.tif")
bio10wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7010.tif")
bio11wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7011.tif")
bio12wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7012.tif")
bio13wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7013.tif")
bio14wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7014.tif")
bio15wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7015.tif")
bio16wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7016.tif")
bio17wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7017.tif")
bio18wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7018.tif")
bio19wf4570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f4570_bio_30seconds/mr45bi7019.tif")

#clip biolayers
f4570_bio_1 = crop(bio1wf4570, range)
f4570_bio_2 = crop(bio2wf4570, range)
f4570_bio_3 = crop(bio3wf4570, range)
f4570_bio_4 = crop(bio4wf4570, range)
f4570_bio_5 = crop(bio5wf4570, range)
f4570_bio_6 = crop(bio6wf4570, range)
f4570_bio_7 = crop(bio7wf4570, range)
f4570_bio_8 = crop(bio8wf4570, range)
f4570_bio_9 = crop(bio9wf4570, range)
f4570_bio_10 = crop(bio10wf4570, range)
f4570_bio_11 = crop(bio11wf4570, range)
f4570_bio_12 = crop(bio12wf4570, range)
f4570_bio_13 = crop(bio13wf4570, range)
f4570_bio_14 = crop(bio14wf4570, range)
f4570_bio_15 = crop(bio15wf4570, range)
f4570_bio_16 = crop(bio16wf4570, range)
f4570_bio_17 = crop(bio17wf4570, range)
f4570_bio_18 = crop(bio18wf4570, range)
f4570_bio_19 = crop(bio19wf4570, range)

rm(bio1wf4570, bio2wf4570, bio3wf4570, bio4wf4570, bio5wf4570, bio6wf4570, bio7wf4570, bio8wf4570, bio9wf4570, bio10wf4570, bio11wf4570, bio12wf4570, bio13wf4570, bio14wf4570, bio15wf4570, bio16wf4570, bio17wf4570, bio18wf4570, bio19wf4570)

#get bioclim variables for model
f4570_list<-gsub("bio", "f4570_bio", new_list)

#stack bioclim layers file
f4570_bioclim = eval(parse(text = paste("stack(",f4570_list,")")))
#f4570_bioclim
names(f4570_bioclim)<-c(new_list_q)
#f4570_bioclim

#project model
f4570_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f4570_bioclim,
  proj.name = "f4570",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f4570 <- raster("./NicheModel/proj_f4570/proj_f4570_NicheModel.img")
writeRaster(f4570, file=paste(species,"_f4570",sep=""), format="ascii",overwrite=T)

rm(f4570_bio_1, f4570_bio_2, f4570_bio_3, f4570_bio_4, f4570_bio_5, f4570_bio_6, f4570_bio_7, f4570_bio_8, f4570_bio_9, f4570_bio_10, f4570_bio_11, f4570_bio_12, f4570_bio_13, f4570_bio_14, f4570_bio_15, f4570_bio_16, f4570_bio_17, f4570_bio_18, f4570_bio_19, f4570_list, f4570_bioclim, f4570_projection, f4570)


### Future 6.0 global warming increase model at 50 years
#import bioclim data
bio1wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi501.tif")
bio2wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi502.tif")
bio3wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi503.tif")
bio4wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi504.tif")
bio5wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi505.tif")
bio6wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi506.tif")
bio7wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi507.tif")
bio8wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi508.tif")
bio9wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi509.tif")
bio10wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5010.tif")
bio11wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5011.tif")
bio12wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5012.tif")
bio13wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5013.tif")
bio14wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5014.tif")
bio15wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5015.tif")
bio16wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5016.tif")
bio17wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5017.tif")
bio18wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5018.tif")
bio19wf6050 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6050_bio_30seconds/mr60bi5019.tif")

#clip biolayers
f6050_bio_1 = crop(bio1wf6050, range)
f6050_bio_2 = crop(bio2wf6050, range)
f6050_bio_3 = crop(bio3wf6050, range)
f6050_bio_4 = crop(bio4wf6050, range)
f6050_bio_5 = crop(bio5wf6050, range)
f6050_bio_6 = crop(bio6wf6050, range)
f6050_bio_7 = crop(bio7wf6050, range)
f6050_bio_8 = crop(bio8wf6050, range)
f6050_bio_9 = crop(bio9wf6050, range)
f6050_bio_10 = crop(bio10wf6050, range)
f6050_bio_11 = crop(bio11wf6050, range)
f6050_bio_12 = crop(bio12wf6050, range)
f6050_bio_13 = crop(bio13wf6050, range)
f6050_bio_14 = crop(bio14wf6050, range)
f6050_bio_15 = crop(bio15wf6050, range)
f6050_bio_16 = crop(bio16wf6050, range)
f6050_bio_17 = crop(bio17wf6050, range)
f6050_bio_18 = crop(bio18wf6050, range)
f6050_bio_19 = crop(bio19wf6050, range)

rm(bio1wf6050, bio2wf6050, bio3wf6050, bio4wf6050, bio5wf6050, bio6wf6050, bio7wf6050, bio8wf6050, bio9wf6050, bio10wf6050, bio11wf6050, bio12wf6050, bio13wf6050, bio14wf6050, bio15wf6050, bio16wf6050, bio17wf6050, bio18wf6050, bio19wf6050)

#get bioclim variables for model
f6050_list<-gsub("bio", "f6050_bio", new_list)

#stack bioclim layers file
f6050_bioclim = eval(parse(text = paste("stack(",f6050_list,")")))
#f6050_bioclim
names(f6050_bioclim)<-c(new_list_q)
#f6050_bioclim

#project model
f6050_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f6050_bioclim,
  proj.name = "f6050",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f6050 <- raster("./NicheModel/proj_f6050/proj_f6050_NicheModel.img")
writeRaster(f6050, file=paste(species,"_f6050",sep=""), format="ascii",overwrite=T)

rm(f6050_bio_1, f6050_bio_2, f6050_bio_3, f6050_bio_4, f6050_bio_5, f6050_bio_6, f6050_bio_7, f6050_bio_8, f6050_bio_9, f6050_bio_10, f6050_bio_11, f6050_bio_12, f6050_bio_13, f6050_bio_14, f6050_bio_15, f6050_bio_16, f6050_bio_17, f6050_bio_18, f6050_bio_19, f6050_list, f6050_bioclim, f6050_projection, f6050)


### Future 6.0 global warming increase model at 70 years
#import bioclim data
bio1wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi701.tif")
bio2wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi702.tif")
bio3wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi703.tif")
bio4wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi704.tif")
bio5wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi705.tif")
bio6wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi706.tif")
bio7wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi707.tif")
bio8wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi708.tif")
bio9wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi709.tif")
bio10wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7010.tif")
bio11wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7011.tif")
bio12wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7012.tif")
bio13wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7013.tif")
bio14wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7014.tif")
bio15wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7015.tif")
bio16wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7016.tif")
bio17wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7017.tif")
bio18wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7018.tif")
bio19wf6070 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f6070_bio_30seconds/mr60bi7019.tif")

#clip biolayers
f6070_bio_1 = crop(bio1wf6070, range)
f6070_bio_2 = crop(bio2wf6070, range)
f6070_bio_3 = crop(bio3wf6070, range)
f6070_bio_4 = crop(bio4wf6070, range)
f6070_bio_5 = crop(bio5wf6070, range)
f6070_bio_6 = crop(bio6wf6070, range)
f6070_bio_7 = crop(bio7wf6070, range)
f6070_bio_8 = crop(bio8wf6070, range)
f6070_bio_9 = crop(bio9wf6070, range)
f6070_bio_10 = crop(bio10wf6070, range)
f6070_bio_11 = crop(bio11wf6070, range)
f6070_bio_12 = crop(bio12wf6070, range)
f6070_bio_13 = crop(bio13wf6070, range)
f6070_bio_14 = crop(bio14wf6070, range)
f6070_bio_15 = crop(bio15wf6070, range)
f6070_bio_16 = crop(bio16wf6070, range)
f6070_bio_17 = crop(bio17wf6070, range)
f6070_bio_18 = crop(bio18wf6070, range)
f6070_bio_19 = crop(bio19wf6070, range)

rm(bio1wf6070, bio2wf6070, bio3wf6070, bio4wf6070, bio5wf6070, bio6wf6070, bio7wf6070, bio8wf6070, bio9wf6070, bio10wf6070, bio11wf6070, bio12wf6070, bio13wf6070, bio14wf6070, bio15wf6070, bio16wf6070, bio17wf6070, bio18wf6070, bio19wf6070)

#get bioclim variables for model
f6070_list<-gsub("bio", "f6070_bio", new_list)

#stack bioclim layers file
f6070_bioclim = eval(parse(text = paste("stack(",f6070_list,")")))
#f6070_bioclim
names(f6070_bioclim)<-c(new_list_q)
#f6070_bioclim

#project model
f6070_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f6070_bioclim,
  proj.name = "f6070",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f6070 <- raster("./NicheModel/proj_f6070/proj_f6070_NicheModel.img")
writeRaster(f6070, file=paste(species,"_f6070",sep=""), format="ascii",overwrite=T)

rm(f6070_bio_1, f6070_bio_2, f6070_bio_3, f6070_bio_4, f6070_bio_5, f6070_bio_6, f6070_bio_7, f6070_bio_8, f6070_bio_9, f6070_bio_10, f6070_bio_11, f6070_bio_12, f6070_bio_13, f6070_bio_14, f6070_bio_15, f6070_bio_16, f6070_bio_17, f6070_bio_18, f6070_bio_19, f6070_list, f6070_bioclim, f6070_projection, f6070)


### Future 8.5 global warming increase model at 50 years
#import bioclim data
bio1wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi501.tif")
bio2wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi502.tif")
bio3wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi503.tif")
bio4wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi504.tif")
bio5wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi505.tif")
bio6wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi506.tif")
bio7wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi507.tif")
bio8wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi508.tif")
bio9wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi509.tif")
bio10wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5010.tif")
bio11wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5011.tif")
bio12wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5012.tif")
bio13wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5013.tif")
bio14wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5014.tif")
bio15wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5015.tif")
bio16wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5016.tif")
bio17wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5017.tif")
bio18wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5018.tif")
bio19wf8550 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8550_bio_30seconds/mr85bi5019.tif")

#clip biolayers
f8550_bio_1 = crop(bio1wf8550, range)
f8550_bio_2 = crop(bio2wf8550, range)
f8550_bio_3 = crop(bio3wf8550, range)
f8550_bio_4 = crop(bio4wf8550, range)
f8550_bio_5 = crop(bio5wf8550, range)
f8550_bio_6 = crop(bio6wf8550, range)
f8550_bio_7 = crop(bio7wf8550, range)
f8550_bio_8 = crop(bio8wf8550, range)
f8550_bio_9 = crop(bio9wf8550, range)
f8550_bio_10 = crop(bio10wf8550, range)
f8550_bio_11 = crop(bio11wf8550, range)
f8550_bio_12 = crop(bio12wf8550, range)
f8550_bio_13 = crop(bio13wf8550, range)
f8550_bio_14 = crop(bio14wf8550, range)
f8550_bio_15 = crop(bio15wf8550, range)
f8550_bio_16 = crop(bio16wf8550, range)
f8550_bio_17 = crop(bio17wf8550, range)
f8550_bio_18 = crop(bio18wf8550, range)
f8550_bio_19 = crop(bio19wf8550, range)

rm(bio1wf8550, bio2wf8550, bio3wf8550, bio4wf8550, bio5wf8550, bio6wf8550, bio7wf8550, bio8wf8550, bio9wf8550, bio10wf8550, bio11wf8550, bio12wf8550, bio13wf8550, bio14wf8550, bio15wf8550, bio16wf8550, bio17wf8550, bio18wf8550, bio19wf8550)

#get bioclim variables for model
f8550_list<-gsub("bio", "f8550_bio", new_list)

#stack bioclim layers file
f8550_bioclim = eval(parse(text = paste("stack(",f8550_list,")")))
#f8550_bioclim
names(f8550_bioclim)<-c(new_list_q)
#f8550_bioclim

#project model
f8550_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f8550_bioclim,
  proj.name = "f8550",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f8550 <- raster("./NicheModel/proj_f8550/proj_f8550_NicheModel.img")
writeRaster(f8550, file=paste(species,"_f8550",sep=""), format="ascii",overwrite=T)

rm(f8550_bio_1, f8550_bio_2, f8550_bio_3, f8550_bio_4, f8550_bio_5, f8550_bio_6, f8550_bio_7, f8550_bio_8, f8550_bio_9, f8550_bio_10, f8550_bio_11, f8550_bio_12, f8550_bio_13, f8550_bio_14, f8550_bio_15, f8550_bio_16, f8550_bio_17, f8550_bio_18, f8550_bio_19, f8550_list, f8550_bioclim, f8550_projection, f8550)


### Future 8.5 global warming increase model at 70 years
#import bioclim data
bio1wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi701.tif")
bio2wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi702.tif")
bio3wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi703.tif")
bio4wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi704.tif")
bio5wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi705.tif")
bio6wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi706.tif")
bio7wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi707.tif")
bio8wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi708.tif")
bio9wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi709.tif")
bio10wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7010.tif")
bio11wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7011.tif")
bio12wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7012.tif")
bio13wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7013.tif")
bio14wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7014.tif")
bio15wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7015.tif")
bio16wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7016.tif")
bio17wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7017.tif")
bio18wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7018.tif")
bio19wf8570 = raster("/Volumes/HD2/Tara2016/BIOCLIM/f8570_bio_30seconds/mr85bi7019.tif")

#clip biolayers
f8570_bio_1 = crop(bio1wf8570, range)
f8570_bio_2 = crop(bio2wf8570, range)
f8570_bio_3 = crop(bio3wf8570, range)
f8570_bio_4 = crop(bio4wf8570, range)
f8570_bio_5 = crop(bio5wf8570, range)
f8570_bio_6 = crop(bio6wf8570, range)
f8570_bio_7 = crop(bio7wf8570, range)
f8570_bio_8 = crop(bio8wf8570, range)
f8570_bio_9 = crop(bio9wf8570, range)
f8570_bio_10 = crop(bio10wf8570, range)
f8570_bio_11 = crop(bio11wf8570, range)
f8570_bio_12 = crop(bio12wf8570, range)
f8570_bio_13 = crop(bio13wf8570, range)
f8570_bio_14 = crop(bio14wf8570, range)
f8570_bio_15 = crop(bio15wf8570, range)
f8570_bio_16 = crop(bio16wf8570, range)
f8570_bio_17 = crop(bio17wf8570, range)
f8570_bio_18 = crop(bio18wf8570, range)
f8570_bio_19 = crop(bio19wf8570, range)

rm(bio1wf8570, bio2wf8570, bio3wf8570, bio4wf8570, bio5wf8570, bio6wf8570, bio7wf8570, bio8wf8570, bio9wf8570, bio10wf8570, bio11wf8570, bio12wf8570, bio13wf8570, bio14wf8570, bio15wf8570, bio16wf8570, bio17wf8570, bio18wf8570, bio19wf8570)

#get bioclim variables for model
f8570_list<-gsub("bio", "f8570_bio", new_list)

#stack bioclim layers file
f8570_bioclim = eval(parse(text = paste("stack(",f8570_list,")")))
#f8570_bioclim
names(f8570_bioclim)<-c(new_list_q)
#f8570_bioclim

#project model
f8570_projection <- BIOMOD_Projection(
  modeling.output = BiomodModelOut,
  new.env = f8570_bioclim,
  proj.name = "f8570",
  selected.models = 'all',
  binary.meth = NULL,
  compress = 'xz',
  build.clamping.mask = FALSE,
  output.format = '.img')
  
#export the projection to a raster ascii file for later use
f8570 <- raster("./NicheModel/proj_f8570/proj_f8570_NicheModel.img")
writeRaster(f8570, file=paste(species,"_f8570",sep=""), format="ascii",overwrite=T)


#remove everything from R memory
rm(list = ls())