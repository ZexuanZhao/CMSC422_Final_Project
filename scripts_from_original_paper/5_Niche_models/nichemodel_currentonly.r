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
bio1w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_1.bil")
bio2w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_2.bil")
bio3w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_3.bil")
bio4w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_4.bil")
bio5w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_5.bil")
bio6w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_6.bil")
bio7w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_7.bil")
bio8w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_8.bil")
bio9w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_9.bil")
bio10w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_10.bil")
bio11w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_11.bil")
bio12w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_12.bil")
bio13w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_13.bil")
bio14w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_14.bil")
bio15w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_15.bil")
bio16w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_16.bil")
bio17w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_17.bil")
bio18w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_18.bil")
bio19w = raster("~/Desktop/BIOCLIM/bio_30seconds/bio_19.bil")

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
#remove to free memory
#rm(s, f)

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
                                     resp.xy = gpspoints,
                                     resp.name = "NicheModel",
                                     PA.nb.rep=1,
                                     PA.nb.absences=10000,
                                     PA.strategy="random")

#define parameters for MaxEnt model
BiomodOption <- BIOMOD_ModelingOptions(
  MAXENT = list( path_to_maxent.jar = "~/Desktop/BIOCLIM",
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
  models = c('MAXENT'),
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

#remove everything from R memory
rm(list = ls())