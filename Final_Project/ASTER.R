library(rasterVis)
library(raster)
library(tidyverse)
library(viridis)
library(gridExtra)
library(shiny)
library(shinyjs)
library(imager)
library(rgdal)
library(terra)
library(randomForest)
library(e1071)
library(rgdal)

#grabbing all aster files
directory_path <- "../../ASTER/2005_10_01/AST_L1B_00310012005182513_20231130134446_13225/"

# Use list.files with pattern argument to filter files
tif_files <- list.files(directory_path, pattern = "\\.tif$", full.names = TRUE)

# Print the list of filtered TIFF files
print(tif_files)

#turning into raster
band1 <- rast(tif_files[12])
band2 <- rast(tif_files[13])
band3B <- rast(tif_files[14])
band3N <- rast(tif_files[15])
band4 <- rast(tif_files[1])
band5 <- rast(tif_files[2])
band6 <- rast(tif_files[3])
band7 <- rast(tif_files[4])
band8 <- rast(tif_files[5])
band9 <- rast(tif_files[6])

#rectifying
band1 <- rectify(band1)
band2 <- rectify(band2)
band3B <- rectify(band3B)
band3N <- rectify(band3N)
band4 <- rectify(band4)
band5 <- rectify(band5)
band6 <- rectify(band6)
band7 <- rectify(band7)
band8 <- rectify(band8)
band9 <- rectify(band9)

#setting extent
e <- as(extent(360000, 390000, 4235000, 4265000),'SpatialPolygons')

#cropping all by extent
band1 <- crop(band1,e)
band2 <- crop(band2,e)
band3N <- crop(band3N,e)
band3B <- crop(band3B,e)
band4 <- crop(band4,e)
band5 <- crop(band5,e)
band6 <- crop(band6,e)
band7 <- crop(band7,e)
band8 <- crop(band8,e)
band9 <- crop(band9,e)

#aggregating to make all bands have the same resolution
band1 <- aggregate(band1,2)
band2 <- aggregate(band2,2)
band3B <- aggregate(band3B,2)
band3N <- aggregate(band3N,2)


#resamplling pixels
band4 <- resample(band4,band1)
band5 <- resample(band5,band1)
band6 <- resample(band6,band1)
band7 <- resample(band7,band1)
band8 <- resample(band8,band1)
band9 <- resample(band9,band1)

#stacking all bands
aster <-  c(band1,band2,band3B,band3N,band4,band5,band6,band7,band8,band9)

raster <- raster(aster)
writeRaster(aster, "../../ASTER/2005_10_01/full_cleaned.tif")

#band 5/7
band11 <- (aster[[6]]/aster[[8]])

#band 5/4
band12 <- (aster[[6]]/aster[[1]])

#band 4/5
band13 <- (aster[[5]]/aster[[6]])

#b6/b8 * b9/b8 calcite index
band14 <- (aster[[7]]/aster[[9]])*(aster[[10]]/aster[[9]])

#(b7+b9)/b8 kaolinite, sericite, chlorite and epidoteminerals,
band15 <- (aster[[8]]+aster[[10]])/aster[[9]]

#adding all new ratios
aster2 <- c(aster,band11,band12,band13,band14,band15)

#looking at alteration zones
plotRGB(aster2,11,12,4,stretch="lin",main = "Alteration zones",axes=TRUE)

# off whitish cream color represents hydrothermal alteration
levelplot(aster2[[13]], 
          main = "Hydrothermal Alteration", stretch = "lin")

#calcite index
levelplot(aster2[[14]],
          main = "Calcite index", stretch = "lin")

#kaolinite, sericite, chlorite and epidoteminerals,
levelplot(aster2[[15]],
          main = "kaolinite, sericite, chlorite and epidote minerals", stretch = "lin")

